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
request_map_description <- function(values, prompt) {
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

register_ai_observer <- function(input, output, session, map_data, climate_names, request = request_map_description) {
  content <- shiny::reactiveVal(NULL)
  # Successful answers are reused only within this session and for identical data
  # and prompts. Bound the cache, and never cache errors or invalid answers.
  cache <- list()
  current_key <- NULL
  show_answer <- function(answer) {
    content(typedjs::typed(format_ai_description(answer), contentType = "html",
      typeSpeed = 8, showCursor = FALSE, loop = FALSE))
  }
  output$ia_map_description <- shiny::renderUI(content())
  task <- shiny::ExtendedTask$new(function(values, prompt) {
    pending <- tryCatch(promises::promise_resolve(request(values, prompt)),
      error = function(e) promises::promise_reject(e))
    promises::then(pending,
      onFulfilled = function(answer) list(answer = answer),
      onRejected = function(e) list(error = e))
  }) |> bslib::bind_task_button("ia_map", session = session)

  shiny::observeEvent(input$ia_map, {
    if (task$status() == "running") return()
    shiny::req(input$indicator, input$month, input$year)
    label <- climate_names$label[match(input$indicator, climate_names$name)]
    unit <- climate_names$unit[match(input$indicator, climate_names$name)]
    values <- sf::st_drop_geometry(map_data())[, c("name_mun", "name_uf", "value")]
    content(shiny::tagList(
      shiny::p(shiny::icon("spinner", class = "fa-spin"), " Consultando a IA PCDaS...", role = "status"),
      shiny::p("A análise pode levar alguns instantes.", class = "text-muted")
    ))
    shiny::showModal(shiny::modalDialog(
      title = shiny::tags$img(src = "image_IA_PCDaS.png", alt = "IA PCDaS", style = "width: 160px; max-width: 100%;"),
      shiny::uiOutput("ia_map_description"), size = "l", easyClose = TRUE,
      footer = shiny::modalButton("Fechar")
    ), session = session)
    if (!any(is.finite(values$value))) {
      content(shiny::p("Não há dados climáticos disponíveis para analisar nesta seleção.", role = "status"))
      return()
    }
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
