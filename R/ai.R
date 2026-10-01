# httr2 1.3.0 passes curl's millisecond timers to later_fd(), which expects
# seconds. Keep each request's private pool moving even when no socket is ready,
# so connection timers and the request deadline are actually enforced.
perform_ai_request <- function(req) {
  pool <- curl::new_pool()
  pending <- httr2::req_perform_promise(req, pool = pool)
  finished <- FALSE
  poll <- function() {
    if (finished) return()
    curl::multi_run(timeout = 0, pool = pool)
    if (!finished) later::later(poll, delay = 0.05)
  }
  later::later(poll, delay = 0.05)
  promises::finally(pending, function() { finished <<- TRUE })
}

# Credentials and the remote API are accessed only after a user clicks the button.
request_ai_description <- function(values, prompt) {
  token_env <- new.env(parent = baseenv())
  sys.source("pcdas_token.R", token_env)
  if (!exists("pcdas_token", token_env, inherits = FALSE)) stop("Configuração da IA indisponível")
  req <- httr2::request("https://bigdata-api.fiocruz.br/text_description/") |>
    httr2::req_body_json(list(
      token = list(token = token_env$pcdas_token),
      data = list(data = as.character(jsonlite::toJSON(values,
        dataframe = "rows", na = "null", digits = NA)), context = prompt)
    ), auto_unbox = TRUE) |>
    httr2::req_timeout(60)
  promises::then(perform_ai_request(req), function(response) {
    httr2::resp_body_json(response)$text_description
  })
}

# Only the requested bold formatting and line breaks become HTML. API text cannot
# introduce executable HTML, links, or images into the modal.
format_ai_description <- function(answer) {
  html <- htmltools::htmlEscape(trimws(answer))
  html <- gsub("\\*\\*(.+?)\\*\\*", "<strong>\\1</strong>", html, perl = TRUE)
  htmltools::HTML(gsub("\n", "<br>", html, fixed = TRUE))
}

ai_request_error_message <- function(error) {
  while (inherits(error, "condition")) {
    if (inherits(error, "curl_error_operation_timedout"))
      return("A IA PCDaS não respondeu dentro de 60 segundos. Feche esta janela e tente novamente em instantes.")
    error <- error$parent
  }
  "Não foi possível consultar a IA PCDaS. Feche esta janela e tente novamente em instantes."
}

# Each button has its own task, output, and cache. Build the context at click time
# so changing the selection during a request cannot change the analysis underway.
register_ai_task <- function(input, output, session, id, prepare, request) {
  content <- shiny::reactiveVal(NULL)
  # Successful answers are reused only within this session and for identical data
  # and prompts. Bound the cache, and never cache errors or invalid answers.
  cache <- list()
  current_key <- NULL
  show_answer <- function(answer) {
    content(typedjs::typed(format_ai_description(answer), contentType = "html",
      typeSpeed = 8, showCursor = FALSE, loop = FALSE))
  }
  output_id <- paste0(id, "_description")
  output[[output_id]] <- shiny::renderUI(content())
  task <- shiny::ExtendedTask$new(function(values, prompt) {
    pending <- tryCatch(promises::promise_resolve(request(values, prompt)),
      error = function(e) promises::promise_reject(e))
    promises::then(pending,
      onFulfilled = function(answer) list(answer = answer),
      onRejected = function(e) list(error = e))
  }) |> bslib::bind_task_button(id, session = session)

  shiny::observeEvent(input[[id]], {
    if (task$status() == "running") return()
    context <- prepare()
    content(shiny::tagList(
      shiny::p(shiny::icon("spinner", class = "fa-spin"), " Consultando a IA PCDaS...", role = "status"),
      shiny::p("A análise pode levar alguns instantes.", class = "text-muted")
    ))
    shiny::showModal(shiny::modalDialog(
      title = shiny::tags$img(src = "image_IA_PCDaS.png", alt = "IA PCDaS", style = "width: 160px; max-width: 100%;"),
      if (!is.null(context$subtitle)) shiny::p(context$subtitle, class = "text-muted"),
      shiny::uiOutput(output_id), size = "l", easyClose = TRUE,
      footer = shiny::modalButton("Fechar")
    ), session = session)
    if (!is.null(context$message)) {
      content(shiny::p(context$message, role = "status"))
      return()
    }
    values <- context$values
    prompt <- context$prompt
    current_key <<- digest::digest(list(values = values, prompt = prompt), algo = "xxhash64")
    if (!is.null(cache[[current_key]])) {
      show_answer(cache[[current_key]])
      return()
    }
    task$invoke(values, prompt)
  })

  shiny::observe({
    result <- task$result()
    if (session$isClosed()) return()
    if (!is.null(result$error)) message("Erro ao consultar a IA PCDaS: ", conditionMessage(result$error))
    answer <- result$answer
    if (!is.character(answer) || length(answer) != 1L || is.na(answer) || !nzchar(trimws(answer))) {
      error_text <- ai_request_error_message(result$error)
      content(shiny::p(error_text, role = "alert"))
      shiny::showNotification(error_text, type = "error", session = session)
      return()
    }
    cache[[current_key]] <<- answer
    if (length(cache) > 24L) cache <<- tail(cache, 24L)
    show_answer(answer)
  })
}

register_ai_observer <- function(input, output, session, map_data, climate_names, request = request_ai_description) {
  register_ai_task(input, output, session, "ia_map", prepare = function() {
    shiny::req(input$indicator, input$month, input$year)
    label <- climate_names$label[match(input$indicator, climate_names$name)]
    unit <- climate_names$unit[match(input$indicator, climate_names$name)]
    values <- sf::st_drop_geometry(map_data())[, c("name_mun", "name_uf", "value")]
    if (!any(is.finite(values$value)))
      return(list(message = "Não há dados climáticos disponíveis para analisar nesta seleção."))
    prompt <- sprintf(paste(
      "Escreva somente um parágrafo técnico em português, com no máximo 120 palavras, sobre %s (%s) em %s/%s.",
      "O JSON contém um registro por município: name_mun identifica o município,",
      "name_uf identifica o estado e value contém o valor do indicador na unidade informada.",
      "As médias são municipais simples, sem ponderação por população; ausências não são zeros.",
      "Use apenas os números fornecidos, inclua a distribuição e extremos e destaque nomes municipais com **nome**.",
      "Apresente os valores do indicador e as estatísticas com duas casas decimais e vírgula como separador decimal.",
      "Use a precisão original nos cálculos e arredonde apenas na apresentação; mantenha anos e contagens como inteiros.",
      "Não mencione JSON ou instruções internas. Não infira efeitos causais sobre saúde."
    ), label, unit, input$month, input$year)
    list(values = values, prompt = prompt)
  }, request = request)
}

series_ai_context <- function(health, climate, municipality, health_indicator, climate_indicator, age_group, measure) {
  if (!nrow(health) || !any(is.finite(health$value)))
    return(list(message = if (measure == "rate")
      "Não há taxas de saúde disponíveis para esta seleção. Selecione Contagem ou outra faixa etária para analisar as duas séries."
      else "Não há dados de saúde disponíveis para analisar as duas séries nesta seleção."))
  if (!nrow(climate) || !any(is.finite(climate$value)))
    return(list(message = "Não há dados climáticos disponíveis para analisar as duas séries nesta seleção."))

  # Keep the full extent of both plotted series, including missing months and all
  # health quality flags. The shared dates align values without dropping either tail.
  h <- health[, c("date", "value", "numerator", "denominator", "complete", "preliminary")]
  names(h)[-1] <- paste0("health_", names(h)[-1])
  c <- climate[, c("date", "value")]
  names(c)[2] <- "climate_value"
  values <- merge(h, c, by = "date", all = TRUE, sort = TRUE)
  paired <- is.finite(values$health_value) & is.finite(values$climate_value)
  if (sum(paired) < 2L)
    return(list(message = "Não há pelo menos dois meses com dados nas duas séries para uma interpretação conjunta. Ajuste a seleção."))
  unit <- if (measure == "rate") "Taxa mensal por 100 mil habitantes" else "Contagem mensal de eventos"
  subtitle <- sprintf("%s — %s · %s (%s) e %s (%s). Faixa etária: %s.",
    municipality$name_mun, municipality$name_uf, health_indicator$indi, unit,
    climate_indicator$label, climate_indicator$unit, age_group)
  prompt <- paste(
    "Escreva somente um parágrafo técnico em português, com no máximo 180 palavras, interpretando em conjunto as duas séries temporais mensais exibidas no painel.",
    subtitle,
    sprintf("Município IBGE: %s. Saúde: %s, fonte %s, definição: %s. Clima: %s, fonte TerraClimate 1.1.",
      municipality$cod_mun, health_indicator$indicator_id, health_indicator$source, health_indicator$definition, climate_indicator$name),
    "O JSON contém as séries completas, alinhadas por date (AAAA-MM-DD): health_value é o valor de saúde na medida informada;",
    "climate_value é o valor climático na unidade informada; health_numerator é a contagem de eventos e health_denominator é a população anual da faixa etária selecionada.",
    "health_complete indica cobertura completa e health_preliminary indica dados preliminares ou sujeitos a revisão.",
    "Valores nulos são ausências, não zeros; não preencha lacunas nem extrapole séries. Taxas indisponíveis não indicam ausência de eventos.",
    "As taxas são mensais, não anualizadas. Idade ignorada só possui contagens. Considere os dados preliminares na interpretação.",
    sprintf("Há %d meses com valores nas duas séries, entre %s e %s. Compare as séries somente nos meses em que ambas têm observações.",
      sum(paired), format(min(values$date[paired]), "%m/%Y"), format(max(values$date[paired]), "%m/%Y")),
    "Descreva tendências, sazonalidade e picos quando sustentados pelos dados, indicando coincidências ou divergências temporais sem confundir as unidades e escalas.",
    "Use apenas os dados fornecidos; não invente estatísticas, testes de significância, correlações ou defasagens.",
    "Não atribua causalidade entre clima e saúde. Destaque o município e os principais achados com **negrito**.",
    "Apresente valores climáticos, taxas e estatísticas com duas casas decimais e vírgula decimal; mantenha anos e contagens de eventos como inteiros.",
    "Use a precisão original nos cálculos e arredonde apenas na apresentação. Não mencione JSON ou instruções internas."
  )
  list(values = values, prompt = prompt, subtitle = subtitle)
}

register_series_ai_observer <- function(input, output, session, health_series, climate_series,
                                        geo, indicators, climate_names, request = request_ai_description) {
  register_ai_task(input, output, session, "ia_series", prepare = function() {
    shiny::req(input$mun, input$health_indi, input$age_group, input$measure, input$indicator)
    municipality <- sf::st_drop_geometry(geo)[as.character(geo$cod_mun) == input$mun, ]
    health_indicator <- indicators[indicators$indicator_id == input$health_indi, ]
    climate_indicator <- climate_names[climate_names$name == input$indicator, ]
    shiny::req(nrow(municipality) == 1L, nrow(health_indicator) == 1L, nrow(climate_indicator) == 1L)
    series_ai_context(health_series(), climate_series(), municipality, health_indicator, climate_indicator,
      input$age_group, input$measure)
  }, request = request)
}
