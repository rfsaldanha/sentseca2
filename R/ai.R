# Credentials and the remote API are accessed only after a user clicks the button.
request_map_description <- function(values, prompt) {
  token_env <- new.env(parent = baseenv())
  sys.source("pcdas_token.R", token_env)
  if (!exists("pcdas_token", token_env, inherits = FALSE)) stop("Configuração da IA indisponível")
  rpcdas::get_text_description(df = values, prompt = prompt, pcdas_token = token_env$pcdas_token)
}

register_ai_observer <- function(input, session, map_data, climate_names, request = request_map_description) {
  shiny::observeEvent(input$ia_map, {
    label <- climate_names$label[match(input$indicator, climate_names$name)]
    values <- sf::st_drop_geometry(map_data())[, c("name_mun", "name_uf", "value")]
    answer <- tryCatch(request(values,
      sprintf("Descreva tecnicamente, em português, os valores municipais de %s em %s/%s. Use somente os dados fornecidos; não infira efeitos causais sobre saúde.",
              label, input$month, input$year)), error = function(e) {
      message("Erro ao consultar a IA PCDaS: ", conditionMessage(e))
      NULL
    })
    if (!is.character(answer) || length(answer) != 1L || is.na(answer) || !nzchar(trimws(answer))) {
      shiny::showNotification("Não foi possível consultar a IA PCDaS. Tente novamente em instantes.", type = "error", session = session)
      return()
    }
    shiny::showModal(shiny::modalDialog(title = "IA PCDaS", shiny::p(answer), easyClose = TRUE,
      footer = shiny::modalButton("Fechar")), session = session)
  })
}
