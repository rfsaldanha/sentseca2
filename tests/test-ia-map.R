# Run from the repository root: Rscript tests/test-ia-map.R
suppressPackageStartupMessages({
  library(shiny)
  library(testthat)
})
source("R/ai.R")

# Deferred fixtures exercise loading, errors, and retries without credentials/API calls.
state <- new.env()
state$calls <- 0L
state$modals <- list()
state$notifications <- list()
state$button <- NULL
request <- function(values, prompt) {
  state$calls <- state$calls + 1L
  state$values <- values
  state$prompt <- prompt
  promises::promise(function(resolve, reject) {
    state$resolve <- resolve
    state$reject <- reject
  })
}
climate_names <- data.frame(name = "pdsi", label = "Indicador de teste", unit = "índice")
test_server <- function(input, output, session) {
  map_data <- reactive(data.frame(
    name_mun = "Município de teste", name_uf = "Estado de teste",
    value = if (is.null(input$fixture_value)) 1.25 else input$fixture_value
  ))
  output$month_value <- renderText(input$month)
  register_ai_observer(input, output, session, map_data, climate_names, request = request)
}
mock_session <- MockShinySession$new()
mock_session$sendModal <- function(type, message) {
  state$modals <- append(state$modals, list(list(type = type, message = message)))
}
mock_session$sendNotification <- function(type, message) {
  state$notifications <- append(state$notifications, list(message))
}
mock_session$sendInputMessage <- function(inputId, message) {
  if (inputId == "ia_map") state$button <- message$state
}

testServer(test_server, session = mock_session, {
  settle <- function() {
    for (i in seq_len(50L)) {
      later::run_now(0)
      session$flushReact()
      if (identical(state$button, "ready")) return(invisible(NULL))
    }
    stop("The IA task did not settle")
  }
  session$setInputs(indicator = "pdsi", year = 2022, month = 6)

  test_that("click immediately opens a loading modal and leaves the session responsive", {
    expect_identical(state$calls, 0L)
    session$setInputs(ia_map = 1)
    expect_identical(state$calls, 1L)
    expect_length(state$modals, 1)
    expect_match(state$modals[[1]]$message$html, "image_IA_PCDaS.png")
    expect_match(output$ia_map_description$html, "Consultando a IA PCDaS")
    expect_match(output$ia_map_description$html, "fa-spin")
    expect_identical(state$button, "busy")
    expect_match(state$prompt, "6/2022", fixed = TRUE)
    expect_match(state$prompt, "índice", fixed = TRUE)
    expect_match(state$prompt, "**nome**", fixed = TRUE)
    expect_match(state$prompt, "um registro por município", fixed = TRUE)
    expect_equal(state$values$value, 1.25)
    session$setInputs(month = 7, ia_map = 2)
    expect_identical(output$month_value, "7")
    expect_identical(state$calls, 1L)
    expect_false(session$isClosed())
  })

  test_that("API failure stops loading and allows retry in the same session", {
    state$reject(simpleError("Simulated HTTP 422 from API"))
    expect_message(settle(), "Erro ao consultar a IA PCDaS: Simulated HTTP 422")
    expect_false(session$isClosed())
    expect_identical(state$button, "ready")
    expect_length(state$notifications, 1)
    expect_identical(state$notifications[[1]]$type, "error")
    expect_match(output$ia_map_description$html, "tente novamente")
    expect_false(grepl("Simulated HTTP", output$ia_map_description$html))
    expect_false(grepl("fa-spin", output$ia_map_description$html))
    expect_length(state$modals, 1)
  })

  test_that("empty or invalid API responses stop loading without closing the session", {
    invalid <- list(NULL, character(), "", " \n ", NA_character_, c("a", "b"), 42)
    for (i in seq_along(invalid)) {
      session$setInputs(ia_map = i + 2)
      state$resolve(invalid[[i]])
      settle()
      expect_false(session$isClosed())
      expect_identical(state$button, "ready")
      expect_length(state$notifications, i + 1)
      expect_match(output$ia_map_description$html, "tente novamente")
    }
  })

  test_that("a successful retry replaces loading with animated bold text", {
    session$setInputs(ia_map = 20)
    state$resolve("Análise de **Município de teste**: valor de 1,25.")
    settle()
    expect_identical(state$button, "ready")
    expect_match(output$ia_map_description$html, "typed html-widget")
    widget_json <- sub("(?s).*<script[^>]*>(.*)</script>.*", "\\1", output$ia_map_description$html, perl = TRUE)
    widget <- jsonlite::fromJSON(widget_json)
    expect_match(widget$x$strings, "<strong>Município de teste</strong>", fixed = TRUE)
    expect_false(grepl("Consultando", output$ia_map_description$html))
    expect_length(state$notifications, 8)
    # Completion updates the existing output; it never reopens a dismissed modal.
    expect_length(state$modals, 9)
    session$setInputs(month = 8)
    expect_identical(output$month_value, "8")
    expect_false(session$isClosed())
  })

  test_that("a cached answer starts immediately without another request", {
    calls <- state$calls
    session$setInputs(month = 7, ia_map = 21)
    expect_identical(state$calls, calls)
    expect_match(output$ia_map_description$html, "typed html-widget")
    expect_false(grepl("fa-spin", output$ia_map_description$html))
    expect_identical(state$button, "ready")
  })

  test_that("different periods and changed data trigger fresh requests", {
    calls <- state$calls
    session$setInputs(month = 8, ia_map = 22)
    expect_identical(state$calls, calls + 1L)
    state$resolve("Resposta de teste para agosto.")
    settle()
    session$setInputs(fixture_value = 2.5, ia_map = 23)
    expect_identical(state$calls, calls + 2L)
    state$resolve("Resposta de teste com dados atualizados.")
    settle()
    session$setInputs(fixture_value = NA_real_, ia_map = 24)
    expect_identical(state$calls, calls + 2L)
    expect_match(output$ia_map_description$html, "Não há dados climáticos")
    expect_false(grepl("fa-spin", output$ia_map_description$html))
  })

  test_that("timeouts explain the delay and do not become cached responses", {
    session$setInputs(fixture_value = 1.25, month = 9, ia_map = 25)
    timeout <- structure(list(message = "Simulated timeout", call = NULL),
      class = c("curl_error_operation_timedout", "error", "condition"))
    state$reject(structure(list(message = "HTTP request failed", parent = timeout),
      class = c("httr2_failure", "error", "condition")))
    expect_message(settle(), "HTTP request failed")
    expect_match(output$ia_map_description$html, "60 segundos")
    expect_identical(state$button, "ready")
    calls <- state$calls
    session$setInputs(ia_map = 26)
    expect_identical(state$calls, calls + 1L)
    state$resolve("Resposta de teste após timeout.")
    settle()
  })
})

test_that("formatting preserves bold text and escapes remote HTML", {
  html <- as.character(format_ai_description('**Teste**\n<script>alert(1)</script><img src=x onerror=alert(1)>'))
  expect_match(html, "<strong>Teste</strong><br>", fixed = TRUE)
  expect_match(html, "&lt;script&gt;", fixed = TRUE)
  expect_false(grepl("<script|<img", html))
})

# Test the actual HTTP adapter with a fake token and a mocked transport.
test_that("the API adapter sends every municipal record, missing values, and full precision", {
  values <- data.frame(name_mun = paste("Município de teste", seq_len(1500)),
    name_uf = rep(c("Estado de teste A", "Estado de teste B"), 750),
    value = rep(c(-4, 0, 10, 2, NA, 1.23456789012345), 250))
  adapter_env <- new.env(parent = environment(request_ai_description))
  adapter_env$sys.source <- function(file, envir) {
    expect_identical(file, "pcdas_token.R")
    envir$pcdas_token <- "test-token"
  }
  adapter <- request_ai_description
  environment(adapter) <- adapter_env
  done <- FALSE
  actual <- NULL
  httr2::with_mocked_responses(function(req) {
    expect_identical(req$url, "https://bigdata-api.fiocruz.br/text_description/")
    body <- req$body$data
    expect_identical(body$token$token, "test-token")
    expect_identical(body$data$context, "Prompt de teste")
    expect_true(is.character(body$data$data))
    sent <- jsonlite::fromJSON(body$data$data)
    expect_equal(nrow(sent), nrow(values))
    expect_identical(names(sent), names(values))
    expect_equal(sent, values, tolerance = 1e-14)
    expect_identical(is.na(sent$value), is.na(values$value))
    expect_match(body$data$data, '"value":null', fixed = TRUE)
    expect_equal(req$options$timeout_ms, 60000)
    httr2::response(status_code = 200, headers = list(`content-type` = "application/json"),
      body = charToRaw('{"text_description":"Resposta de teste"}'))
  }, {
    pending <- adapter(values, "Prompt de teste")
    expect_true(promises::is.promise(pending))
    promises::then(pending, function(answer) { actual <<- answer; done <<- TRUE })
    for (i in seq_len(50L)) {
      later::run_now(0)
      if (done) break
    }
  })
  expect_true(done)
  expect_identical(actual, "Resposta de teste")
})
