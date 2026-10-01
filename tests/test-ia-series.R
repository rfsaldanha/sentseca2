# Synthetic fixtures only; never reads credentials or calls the remote API.
suppressPackageStartupMessages({ library(shiny); library(testthat) })
source('R/ai.R')
municipalities <- data.frame(cod_mun = c(1L, 2L), name_mun = c('Município de teste A', 'Município de teste B'),
  name_uf = c('Estado de teste A', 'Estado de teste B'))
indicators <- data.frame(indicator_id = c('sinan_dengue', 'sih_asthma'),
  indi = c('Casos confirmados de dengue', 'Internações por asma'),
  source = c('SINAN-DENGUE', 'SIH-RD'), definition = c('Classificação final de dengue', 'J45–J46'))
climate_names <- data.frame(name = c('pdsi', 'ppt'), label = c('Índice climático de teste', 'Precipitação'), unit = c('índice', 'mm'))
health <- data.frame(date = seq(as.Date('2020-03-01'), by = 'month', length.out = 5),
  value = c(3, NA, 0, 4, 5), numerator = c(3L, NA, 0L, 4L, 5L),
  denominator = c(100, NA, 200, NA, 300), complete = c(TRUE, NA, TRUE, FALSE, TRUE),
  preliminary = c(FALSE, NA, FALSE, TRUE, TRUE))
climate <- data.frame(date = seq(as.Date('2020-01-01'), by = 'month', length.out = 5),
  value = c(1.23456789012345, 0, -1.2, 7, 2.56))
context <- function(h = health, c = climate, measure = 'count')
  series_ai_context(h, c, municipalities[1, ], indicators[1, ], climate_names[1, ], 'Total', measure)

test_that('both full series are aligned by month without losing tails, gaps, zeros, precision or flags', {
  x <- context()
  expect_null(x$message)
  expect_equal(x$values$date, seq(as.Date('2020-01-01'), by = 'month', length.out = 7))
  expect_identical(x$values$climate_value, c(climate$value, NA_real_, NA_real_))
  expect_identical(x$values$health_value, c(NA_real_, NA_real_, health$value))
  expect_identical(x$values$health_numerator, c(NA_integer_, NA_integer_, health$numerator))
  expect_identical(x$values$health_denominator, c(NA_real_, NA_real_, health$denominator))
  expect_identical(x$values$health_complete, c(NA, NA, health$complete))
  expect_identical(x$values$health_preliminary, c(NA, NA, health$preliminary))
  expect_match(x$prompt, 'Há 2 meses com valores nas duas séries, entre 03/2020 e 05/2020', fixed = TRUE)
  expect_match(x$prompt, 'Casos confirmados de dengue', fixed = TRUE)
  expect_match(x$prompt, 'Classificação final de dengue', fixed = TRUE)
  expect_match(x$prompt, 'SINAN-DENGUE', fixed = TRUE)
  expect_match(x$prompt, 'não zeros', fixed = TRUE)
  expect_match(x$prompt, 'Não atribua causalidade', fixed = TRUE)
  expect_match(x$prompt, 'duas casas decimais', fixed = TRUE)
  expect_match(x$prompt, 'contagens de eventos como inteiros', fixed = TRUE)
  expect_match(x$subtitle, 'Contagem mensal de eventos', fixed = TRUE)
  expect_match(context(measure = 'rate')$subtitle, 'Taxa mensal por 100 mil habitantes', fixed = TRUE)
})

test_that('unavailable series and insufficient overlap explain why joint interpretation is unavailable', {
  expect_match(context(health[FALSE, ])$message, 'Não há dados de saúde')
  missing <- health; missing$value[] <- NA_real_
  expect_match(context(missing, measure = 'rate')$message, 'Selecione Contagem')
  expect_match(context(c = climate[FALSE, ])$message, 'Não há dados climáticos')
  missing <- climate; missing$value[] <- NA_real_
  expect_match(context(c = missing)$message, 'Não há dados climáticos')
  disjoint <- climate; disjoint$date <- disjoint$date - 366
  expect_match(context(c = disjoint)$message, 'pelo menos dois meses')
  expect_match(context(c = climate[1:3, ])$message, 'pelo menos dois meses')
  zeros <- health; zeros$value[] <- 0
  expect_null(context(zeros)$message)
})

test_that('the actual adapter serializes every month and preserves nulls and original precision', {
  x <- context()
  adapter <- request_ai_description
  adapter_env <- new.env(parent = environment(adapter))
  adapter_env$sys.source <- function(file, envir) envir$pcdas_token <- 'test-token'
  adapter_env$perform_ai_request <- function(req) {
    sent <- jsonlite::fromJSON(req$body$data$data$data)
    expect_equal(nrow(sent), 7L)
    expect_identical(sent$date, as.character(x$values$date))
    expect_equal(sent$climate_value, x$values$climate_value, tolerance = 1e-14)
    expect_equal(sent$health_value, x$values$health_value)
    expect_identical(sent$health_complete, x$values$health_complete)
    expect_identical(sent$health_preliminary, x$values$health_preliminary)
    expect_match(req$body$data$data$data, '"health_value":null', fixed = TRUE)
    expect_identical(req$body$data$data$context, x$prompt)
    promises::promise_resolve(httr2::response(status_code = 200, headers = list(`content-type` = 'application/json'),
      body = charToRaw('{"text_description":"Resposta simulada"}')))
  }
  environment(adapter) <- adapter_env
  done <- FALSE
  promises::then(adapter(x$values, x$prompt), function(answer) {
    expect_identical(answer, 'Resposta simulada'); done <<- TRUE
  })
  for (i in seq_len(50L)) { later::run_now(0); if (done) break }
  expect_true(done)
})

state <- new.env()
state$requests <- list(); state$buttons <- list(); state$modals <- list()
request <- function(values, prompt) {
  i <- length(state$requests) + 1L
  state$requests[[i]] <- list(values = values, prompt = prompt)
  promises::promise(function(resolve, reject) {
    state$requests[[i]]$resolve <- resolve; state$requests[[i]]$reject <- reject
  })
}
server <- function(input, output, session) {
  h <- reactive({
    x <- health
    if (identical(input$measure, 'rate')) x$value <- x$numerator / x$denominator * 1e5
    if (identical(input$age_group, 'Idade ignorada') && identical(input$measure, 'rate')) x$value[] <- NA_real_
    if (!is.null(input$changed_data)) x$value[1] <- input$changed_data
    x
  })
  register_series_ai_observer(input, output, session, h, reactive(climate),
    municipalities, indicators, climate_names, request = request)
  register_ai_observer(input, output, session,
    reactive(data.frame(name_mun = 'Município de teste A', name_uf = 'Estado de teste A', value = 1)),
    climate_names, request = request)
}
mock <- MockShinySession$new()
mock$sendInputMessage <- function(inputId, message) state$buttons[[inputId]] <- message$state
mock$sendModal <- function(type, message) state$modals <- append(state$modals, list(message$html))

testServer(server, session = mock, {
  settle <- function(id = 'ia_series') {
    for (i in seq_len(50L)) {
      later::run_now(0); session$flushReact()
      if (identical(state$buttons[[id]], 'ready')) return(invisible(NULL))
    }
    stop('IA task did not settle')
  }
  click <- 0L
  analyze <- function() { click <<- click + 1L; session$setInputs(ia_series = click) }
  session$setInputs(mun = '1', indicator = 'pdsi', health_indi = 'sinan_dengue', age_group = 'Total', measure = 'count', year = '2020', month = '3')
  test_that('series and map run independently and preserve the selection made at click time', {
    expect_length(state$requests, 0)
    analyze()
    expect_length(state$requests, 1)
    expect_equal(state$requests[[1]]$values, context()$values)
    expect_match(output$ia_series_description$html, 'fa-spin')
    expect_match(state$modals[[1]], 'Município de teste A', fixed = TRUE)
    expect_identical(state$buttons$ia_series, 'busy')
    session$setInputs(mun = '2')
    analyze()
    expect_length(state$requests, 1)
    expect_match(state$requests[[1]]$prompt, 'Município de teste A', fixed = TRUE)
    session$setInputs(ia_map = 1)
    expect_length(state$requests, 2)
    state$requests[[1]]$resolve('Resposta simulada das **séries**.')
    settle()
    expect_match(output$ia_series_description$html, 'typed html-widget')
    expect_match(output$ia_map_description$html, 'fa-spin')
    expect_identical(state$buttons$ia_map, 'busy')
    state$requests[[2]]$resolve('Resposta simulada do **mapa**.')
    settle('ia_map')
    expect_match(output$ia_map_description$html, 'typed html-widget')
    expect_false(session$isClosed())
  })
  test_that('identical data reuse the series cache even if map month and year change', {
    session$setInputs(mun = '1', year = '2021', month = '6')
    analyze()
    expect_length(state$requests, 2)
    expect_match(output$ia_series_description$html, 'typed html-widget')
    expect_false(grepl('fa-spin', output$ia_series_description$html))
  })
  test_that('each series selection and changed values invalidate the cached answer', {
    changes <- list(list(mun = '2'), list(indicator = 'ppt'), list(health_indi = 'sih_asthma'),
      list(age_group = '65 anos ou mais'), list(measure = 'rate'), list(changed_data = 1.23456789))
    for (change in changes) {
      before <- length(state$requests)
      do.call(session$setInputs, change)
      analyze()
      expect_length(state$requests, before + 1L)
      state$requests[[before + 1L]]$resolve('Resposta simulada para seleção alterada.')
      settle()
    }
    expect_match(tail(state$requests, 1)[[1]]$prompt, 'Taxa mensal por 100 mil habitantes', fixed = TRUE)
  })
  test_that('missing rates skip the API and an API failure allows another attempt', {
    session$setInputs(changed_data = NULL, age_group = 'Idade ignorada')
    before <- length(state$requests)
    analyze()
    expect_length(state$requests, before)
    expect_match(output$ia_series_description$html, 'Selecione Contagem')
    expect_false(grepl('fa-spin', output$ia_series_description$html))
    session$setInputs(measure = 'count')
    analyze()
    state$requests[[before + 1L]]$reject(simpleError('Simulated series failure'))
    expect_message(settle(), 'Simulated series failure')
    expect_match(output$ia_series_description$html, 'tente novamente')
    analyze()
    expect_length(state$requests, before + 2L)
    state$requests[[before + 2L]]$resolve('Resposta simulada após nova tentativa.')
    settle()
    expect_match(output$ia_series_description$html, 'typed html-widget')
    expect_false(session$isClosed())
  })
})
