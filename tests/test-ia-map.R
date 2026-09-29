# Run from the repository root: Rscript tests/test-ia-map.R
suppressPackageStartupMessages({
  library(shiny)
  library(dplyr)
  library(sf)
  library(glue)
  library(htmltools)
  library(testthat)
})

# Exercise the actual observer without production data, credentials or API calls.
args <- commandArgs(trailingOnly = TRUE)
app_file <- if (length(args)) args[[1]] else "app.R"
app_code <- parse(app_file)
server_assignment <- Filter(function(expr) {
  is.call(expr) && identical(expr[[1]], as.name("<-")) &&
    identical(expr[[2]], as.name("server"))
}, as.list(app_code))[[1]]
ia_observer <- Filter(function(expr) {
  is.call(expr) && identical(expr[[1]], as.name("observeEvent")) &&
    identical(expr[[2]], quote(input$ia_map))
}, as.list(server_assignment[[3]][[3]]))
stopifnot(length(ia_observer) == 1L)

state <- new.env()
state$response <- simpleError("Simulated HTTP 422 from API")
state$modals <- list()
state$notifications <- list()
get_text_description <- function(...) {
  if (inherits(state$response, "error")) stop(state$response)
  state$response
}
pcdas_token <- "test-token"
clim_indi_names <- c("Indicador de teste" = "pdsi")

test_server <- function(input, output, session) {}
body(test_server) <- bquote({
  geo_data <- reactive(data.frame(
    name_mun = "Município de teste", name_uf = "Estado de teste", value = 1.25
  ))
  output$month_value <- renderText(input$month)
  .(ia_observer[[1]])
})
mock_session <- MockShinySession$new()
mock_session$sendModal <- function(type, message) {
  state$modals <- append(state$modals, list(list(type = type, message = message)))
}
mock_session$sendNotification <- function(type, message) {
  state$notifications <- append(state$notifications, list(message))
}

testServer(test_server, session = mock_session, {
  session$setInputs(indicator = "pdsi", year = 2022, month = 6)

  test_that("API failure shows an error and keeps the session responsive", {
    expect_message(
      session$setInputs(ia_map = 1),
      "Erro ao consultar a IA PCDaS: Simulated HTTP 422"
    )
    expect_false(session$isClosed())
    expect_length(state$modals, 0)
    expect_length(state$notifications, 1)
    expect_identical(state$notifications[[1]]$type, "error")
    expect_match(state$notifications[[1]]$html, "Tente novamente")
    expect_false(grepl("Simulated HTTP", state$notifications[[1]]$html))
    session$setInputs(month = 7)
    expect_identical(output$month_value, "7")
  })

  test_that("empty or invalid API responses do not open a modal or close the session", {
    invalid <- list(NULL, character(), "", " \n ", NA_character_, c("a", "b"), 42)
    for (i in seq_along(invalid)) {
      state$response <- invalid[[i]]
      session$setInputs(ia_map = i + 1)
      expect_false(session$isClosed())
      expect_length(state$modals, 0)
      expect_length(state$notifications, i + 1)
    }
  })

  test_that("a successful retry displays the description in the same session", {
    state$response <- "Análise de **Município de teste**: valor de 1,25."
    session$setInputs(ia_map = 20)
    expect_false(session$isClosed())
    expect_length(state$modals, 1)
    expect_identical(state$modals[[1]]$type, "show")
    expect_match(state$modals[[1]]$message$html, "Município de teste")
    expect_length(state$notifications, 8)
    session$setInputs(month = 8)
    expect_identical(output$month_value, "8")
  })
})
