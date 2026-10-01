# Uses an actual prepared publication; no full-table deserialization or remote calls.
suppressPackageStartupMessages(library(testthat))
env <- new.env()
env$readRDS <- function(file, ...) {
  if (basename(file) == "health.rds") stop("The dashboard must not load health.rds")
  base::readRDS(file, ...)
}
sys.source("app.R", env)
stopifnot(!is.null(env$dataset))
tryCatch({
  shiny::testServer(env$server, {
    coverage <- env$dataset$climate$coverage
    v <- if ("pdsi" %in% coverage$name) "pdsi" else coverage$name[1]
    period <- tail(coverage[coverage$name == v, ][order(coverage$year[coverage$name == v], coverage$month[coverage$name == v]), ], 1)
    mun <- env$geo$cod_mun[1]
    first_indicator <- env$indicators$indicator_id[1]
    session$setInputs(indicator = v, year = as.character(period$year), month = as.character(period$month),
      mun = as.character(mun), health_indi = first_indicator, age_group = "Total", measure = "count")
    test_that("the map and graphs use the complete current territory", {
      expect_equal(nrow(map_data()), nrow(env$geo))
      expect_gt(nrow(climate_series()), 0)
      expect_gt(nrow(health_series()), 0)
      expect_false("health" %in% names(env$dataset))
      expect_false(session$isClosed())
      expect_true(nzchar(output$graph_health))
    })
    test_that("all climate variables and health indicators can be selected", {
      for (variable in env$climate_names$name) {
        session$setInputs(indicator = variable)
        expect_equal(nrow(map_data()), nrow(env$geo))
        expect_gt(nrow(climate_series()), 0)
      }
      for (id in env$indicators$indicator_id) {
        session$setInputs(health_indi = id, measure = "count", age_group = "Total")
        x <- health_series()
        expect_gt(nrow(x), 0)
        expect_equal(x$value, as.numeric(x$numerator))
        session$setInputs(measure = "rate")
        x <- health_series()
        eligible <- !is.na(x$denominator) & x$denominator > 0 & !is.na(x$complete) & x$complete
        expect_equal(x$value[eligible], x$numerator[eligible] / x$denominator[eligible] * 1e5)
        expect_true(all(is.na(x$value[!eligible])))
      }
    })
    test_that("recent preliminary records and unavailable populations are explicit", {
      session$setInputs(health_indi = "sinan_dengue", measure = "count", age_group = "Total")
      counts <- health_series()
      if (nrow(counts)) {
        latest_year <- max(counts$year, na.rm = TRUE)
        published <- env$dataset$health_metadata$coverage
        expected_preliminary <- published$preliminary[published$source == "SINAN-DENGUE" & published$period == as.character(latest_year)]
        expect_identical(any(counts$preliminary[counts$year == latest_year], na.rm = TRUE), any(expected_preliminary))
        if (any(expected_preliminary)) expect_match(output$health_info, "preliminares")
        session$setInputs(measure = "rate")
        rates <- health_series()
        missing_population <- is.na(rates$denominator)
        expect_true(all(is.na(rates$value[missing_population])))
        if (any(missing_population)) expect_match(output$health_info, "sem dados ou sem denominador")
      }
      session$setInputs(age_group = "Idade ignorada", measure = "rate")
      expect_true(all(is.na(health_series()$value)))
      expect_match(output$health_info, "idade ignorada")
      expect_true(nzchar(output$graph_health))
      session$setInputs(measure = "count")
      expect_gt(nrow(health_series()), 0)
      expect_true(all(!is.na(health_series()$value)))
    })
    test_that("new municipalities and missing selections do not close the session", {
      if (file.exists("data/geo.rds")) {
        previous <- readRDS("data/geo.rds")
        added <- setdiff(env$geo$cod_mun, previous$cod_mun)
        if (length(added)) {
          session$setInputs(mun = as.character(added[1]), age_group = "Total")
          expect_gt(nrow(climate_series()), 0)
          expect_gt(nrow(health_series()), 0)
        }
      }
      session$setInputs(mun = "0")
      expect_equal(nrow(health_series()), 0L)
      expect_match(output$health_info, "Sem dados")
      session$setInputs(mun = as.character(mun))
      expect_gt(nrow(health_series()), 0)
      expect_false(session$isClosed())
    })
  })
  # A restricted catalog must actually update the controls, including invalid prior selections.
  restricted_id <- env$indicators$indicator_id[1]
  restricted_age <- setdiff(env$dataset$metadata$age_groups, c("Total", "Idade ignorada"))[1]
  availability <- env$dataset$availability
  env$dataset$availability <- availability[availability$indicator_id != restricted_id |
    (availability$age_group == restricted_age & availability$measure == "count"), ]
  messages <- new.env(); messages$values <- list()
  mock <- shiny::MockShinySession$new()
  mock$sendInputMessage <- function(inputId, message) messages$values[[inputId]] <- message
  shiny::testServer(env$server, session = mock, {
    session$setInputs(health_indi = restricted_id, age_group = "Total", measure = "rate")
    test_that("restricted catalogs update age and measure choices", {
      expect_equal(messages$values$age_group$value, restricted_age)
      session$setInputs(age_group = restricted_age)
      expect_equal(unname(messages$values$measure$value), "count")
    })
  })
  env$dataset$availability <- availability
}, finally = env$close_dashboard_data(env$dataset))
cat("Shiny smoke checks passed\n")
