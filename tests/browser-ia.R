# Starts the real app with delayed test responses; never reads credentials/calls the API.
# Requires chromote, callr, and Chrome in addition to the app dependencies.
port <- httpuv::randomPort()
project <- normalizePath(".")
log <- tempfile("sentseca-browser-ia-", fileext = ".log")
response <- paste(
  "Resposta simulada para testar a interface: **Município de teste**.",
  "Este texto não é uma análise dos dados e serve apenas para verificar a digitação e o negrito."
)
app <- callr::r_bg(function(project, port, response) {
  setwd(project)
  env <- new.env()
  sys.source("app.R", env)
  stopifnot(!is.null(env$dataset), isTRUE(env$ai_available))
  calls <- 0L
  env$request_map_description <- function(values, prompt) {
    calls <<- calls + 1L
    attempt <- calls
    promises::promise(function(resolve, reject) {
      later::later(function() {
        if (attempt == 2L) reject(simpleError("Simulated API failure")) else resolve(response)
      }, delay = 3)
    })
  }
  test_app <- shiny::shinyApp(env$ui, env$server)
  test_app$staticPaths <- list(`/` = httpuv::staticPath(file.path(project, "www"), indexhtml = FALSE, fallthrough = TRUE))
  shiny::runApp(test_app, host = "127.0.0.1", port = port, launch.browser = FALSE)
}, args = list(project, port, response), stdout = log, stderr = log)
chrome <- NULL
b <- NULL
tryCatch({
  url <- sprintf("http://127.0.0.1:%d", port)
  started <- FALSE
  for (i in seq_len(100L)) {
    if (!app$is_alive()) stop(paste(readLines(log), collapse = "\n"))
    started <- tryCatch({
      curl::curl_fetch_memory(url, handle = curl::new_handle(timeout_ms = 500))
      TRUE
    }, error = function(e) FALSE)
    if (started) break
    Sys.sleep(0.1)
  }
  stopifnot(started)
  chrome <- chromote::Chromote$new(browser = chromote::Chrome$new(args = c("--headless", "--no-sandbox", "--disable-dev-shm-usage")))
  b <- chromote::ChromoteSession$new(parent = chrome)
  js <- function(code) {
    result <- b$Runtime$evaluate(expression = code, returnByValue = TRUE)
    if (!is.null(result$exceptionDetails)) stop("JavaScript evaluation failed: ", result$exceptionDetails$text)
    result$result$value
  }
  wait_for <- function(code, label) {
    for (i in seq_len(120L)) {
      if (isTRUE(js(code))) return(invisible(TRUE))
      Sys.sleep(0.1)
    }
    stop("Browser timeout: ", label)
  }
  b$Emulation$setDeviceMetricsOverride(width = 1440L, height = 1080L, deviceScaleFactor = 1, mobile = FALSE)
  b$Page$addScriptToEvaluateOnNewDocument(source = "window.__dashboardErrors=[];window.addEventListener('error',e=>window.__dashboardErrors.push(e.message));")
  b$Page$navigate(url)
  wait_for("Boolean(window.Shiny && Shiny.shinyapp && Shiny.shinyapp.isConnected() && document.getElementById('ia_map'))", "startup")
  js("$(document).on('shown.bs.modal', '#shiny-modal', function(){this.dataset.testShown='true';}); true")
  stopifnot(isTRUE(js("Boolean(document.querySelector('#ia_map .fa-wand-magic-sparkles'))")))
  js("document.getElementById('ia_map').click(); true")
  wait_for("Boolean(document.querySelector('#shiny-modal #ia_map_description .fa-spin'))", "loading spinner")
  stopifnot(isTRUE(js("document.getElementById('ia_map').disabled")))
  wait_for("document.querySelector('#shiny-modal img')?.naturalWidth > 0", "IA logo")
  stopifnot(isTRUE(js("getComputedStyle(document.querySelector('#ia_map_description .fa-spin')).animationName !== 'none'")))
  stopifnot(isTRUE(js("Boolean(document.querySelector('#ia_map .fa-spin'))")))
  artifacts <- file.path(project, "data", "diagnostics")
  dir.create(artifacts, recursive = TRUE, showWarnings = FALSE)
  b$screenshot(file.path(artifacts, "ia-loading.png"))
  wait_for("Boolean(document.querySelector('#ia_map_description .typed')?.textContent.length)", "typing starts")
  expected <- gsub("**", "", response, fixed = TRUE)
  initial_length <- js("document.querySelector('#ia_map_description .typed').textContent.length")
  stopifnot(initial_length < nchar(expected))
  wait_for(sprintf("document.querySelector('#ia_map_description .typed')?.textContent === %s", jsonlite::toJSON(expected, auto_unbox = TRUE)), "typing completes")
  stopifnot(js("document.querySelector('#ia_map_description strong').textContent") == "Município de teste")
  stopifnot(isTRUE(js("!document.getElementById('ia_map').disabled")))
  stopifnot(js("document.querySelectorAll('#ia_map_description .fa-spin').length") == 0L)
  b$screenshot(file.path(artifacts, "ia-response.png"))

  # Identical selections use the session cache without waiting for the delayed API.
  js("document.querySelector('#shiny-modal .modal-footer button').click(); true")
  wait_for("!document.getElementById('shiny-modal')", "modal closes")
  cached_started <- proc.time()[["elapsed"]]
  js("document.getElementById('ia_map').click(); true")
  wait_for("Boolean(document.querySelector('#ia_map_description .typed'))", "cached answer")
  stopifnot(proc.time()[["elapsed"]] - cached_started < 2)
  stopifnot(js("document.querySelectorAll('#ia_map_description .fa-spin').length") == 0L)
  wait_for("!document.getElementById('ia_map').disabled", "cached button ready")
  wait_for("document.querySelector('#shiny-modal')?.dataset.testShown === 'true'", "cached modal opening transition")
  js("document.querySelector('#shiny-modal .modal-footer button').click(); true")
  wait_for("!document.getElementById('shiny-modal')", "cached modal closes")

  # A different period makes a fresh request. Errors are never cached.
  js("document.getElementById('month').selectize.setValue('11'); true")
  js("document.getElementById('ia_map').click(); true")
  wait_for("Boolean(document.querySelector('#ia_map_description .fa-spin'))", "retry spinner")
  wait_for("Boolean(document.querySelector('#ia_map_description [role=alert]'))", "error message")
  stopifnot(isTRUE(js("!document.getElementById('ia_map').disabled")))
  stopifnot(js("document.querySelectorAll('#ia_map_description .fa-spin').length") == 0L)

  # Closing a pending modal must not cause it to reopen when the answer arrives.
  js("document.querySelector('#shiny-modal .modal-footer button').click(); true")
  wait_for("!document.getElementById('shiny-modal')", "error modal closes")
  js("document.getElementById('ia_map').click(); true")
  wait_for("Boolean(document.querySelector('#ia_map_description .fa-spin'))", "third attempt")
  wait_for("document.querySelector('#shiny-modal')?.dataset.testShown === 'true'", "modal opening transition")
  js("document.querySelector('#shiny-modal .modal-footer button').click(); true")
  wait_for("!document.getElementById('shiny-modal')", "pending modal closes")
  wait_for("!document.getElementById('ia_map').disabled", "background request completes")
  stopifnot(isTRUE(js("!document.getElementById('shiny-modal') && Shiny.shinyapp.isConnected()")))
  stopifnot(length(js("window.__dashboardErrors")) == 0L)
  stopifnot(js("document.querySelectorAll('.shiny-output-error').length") == 0L)
  cat("IA browser checks passed: icon, immediate modal, animated spinners, typed bold response, immediate cached answer, errors, retries, and closed modal stays closed.\n")
}, finally = {
  if (!is.null(b)) b$close()
  if (!is.null(chrome)) chrome$close()
  app$kill()
})
