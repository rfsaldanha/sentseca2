# Start app.R locally first. Never clicks the IA button or reads its credentials.
args <- commandArgs(trailingOnly = TRUE)
url <- if (length(args)) args[1] else "http://127.0.0.1:8765"
source("R/data_access.R")
release <- read_release()
geo <- readRDS(file.path(release$path, "geo.rds"))
climate <- readRDS(file.path(release$path, "climate_metadata.rds"))
latest_year <- max(climate$coverage$year[climate$coverage$name == "pdsi"])
added <- if (file.exists("data/geo.rds")) setdiff(geo$cod_mun, readRDS("data/geo.rds")$cod_mun) else integer()
mun <- if (length(added)) added[1] else geo$cod_mun[1]
artifacts <- "data/diagnostics"
dir.create(artifacts, recursive = TRUE, showWarnings = FALSE)
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
    Sys.sleep(0.25)
  }
  stop("Browser timeout: ", label)
}
select <- function(id, value) {
  js(sprintf("(function(){const el=document.getElementById(%s); if(el.selectize) el.selectize.setValue(%s); else {el.value=%s; el.dispatchEvent(new Event('change',{bubbles:true}));} return true;})()",
    jsonlite::toJSON(id, auto_unbox = TRUE), jsonlite::toJSON(as.character(value), auto_unbox = TRUE), jsonlite::toJSON(as.character(value), auto_unbox = TRUE)))
}
tryCatch({
  b$Emulation$setDeviceMetricsOverride(width = 1440L, height = 1080L, deviceScaleFactor = 1, mobile = FALSE)
  b$Page$addScriptToEvaluateOnNewDocument(source = "window.__dashboardErrors=[];window.addEventListener('error',e=>window.__dashboardErrors.push(e.message));")
  b$Page$navigate(url)
  wait_for("Boolean(window.Shiny && Shiny.shinyapp && Shiny.shinyapp.isConnected() && window.HTMLWidgets && HTMLWidgets.find('#out_map') && HTMLWidgets.find('#out_map').getMap())", "map startup")
  wait_for(sprintf("Object.values(HTMLWidgets.find('#out_map').getMap()._layers).filter(x=>x instanceof L.Polygon).length === %d", nrow(geo)), "territory polygons")
  stopifnot(js("Object.keys(document.getElementById('indicator').selectize.options).length") == 12L)
  stopifnot(js("Object.keys(document.getElementById('health_indi').selectize.options).length") == 9L)
  stopifnot(js("document.getElementById('year').value") == as.character(latest_year))
  b$screenshot(file.path(artifacts, "map.png"))
  js("Array.from(document.querySelectorAll('.nav-link')).find(x=>x.textContent.trim()==='Gráficos').click(); true")
  select("mun", mun); select("health_indi", "sinan_dengue"); select("age_group", "Total")
  wait_for("document.getElementById('health_info').textContent.includes('SINAN-DENGUE')", "indicator change")
  select("measure", "count")
  wait_for("document.getElementById('health_info').textContent.includes('SINAN-DENGUE') && document.getElementById('graph_health').data && document.getElementById('graph_health').data.length >= 2 && document.getElementById('graph_health').data[0].name.includes('Casos prováveis')", "dengue graph")
  wait_for("document.getElementById('graph_health')._fullLayout.yaxis.title.text === 'Número de eventos'", "count axis")
  stopifnot(js("document.getElementById('measure').value") == "count")
  stopifnot(isTRUE(js("document.getElementById('graph_health').data[0].connectgaps === false")))
  stopifnot(isTRUE(js("document.getElementById('health_info').textContent.includes('preliminares')")))
  b$screenshot(file.path(artifacts, "dengue.png"))
  select("measure", "rate")
  wait_for("document.getElementById('health_info').textContent.includes('sem dados ou sem denominador')", "missing population")
  wait_for("document.getElementById('graph_health')._fullLayout.yaxis.title.text === 'Por 100 mil habitantes'", "rate axis")
  select("age_group", "Idade ignorada")
  wait_for("document.getElementById('health_info').textContent.includes('Taxa indisponível para idade ignorada')", "unknown age")
  b$screenshot(file.path(artifacts, "unknown-age.png"))
  select("measure", "count")
  wait_for("document.getElementById('health_info').textContent.includes('SINAN-DENGUE')", "count recovery")
  stopifnot(length(js("window.__dashboardErrors")) == 0L)
  stopifnot(js("document.querySelectorAll('.shiny-output-error').length") == 0L)
  stopifnot(isTRUE(js("Shiny.shinyapp.isConnected()")))
  cat(sprintf("Browser checks passed: %d polygons, 12 climate variables, 9 health indicators; municipality %s; dengue/count/rate/unknown age; no JavaScript errors.\n", nrow(geo), mun))
}, finally = { b$close(); chrome$close() })
