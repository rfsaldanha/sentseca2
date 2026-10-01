suppressPackageStartupMessages(library(testthat))
# Synthetic fixtures exercise storage failures; they are never used by the dashboard.
code <- new.env(parent = globalenv())
sys.source("R/data_access.R", code)
sys.source("R/prepare_data.R", code)
make_fixture <- function() {
  root <- tempfile("dashboard-test-"); dir.create(root)
  path <- file.path(root, "release"); dir.create(path)
  counts <- expand.grid(indicator_id = c("sih_asthma", "sinan_dengue"), cod_mun = c(220005L, 220010L),
    age_group = c("Total", "Idade ignorada"), year = 2025:2026, month = c(1L, 3L), stringsAsFactors = FALSE)
  counts$source <- ifelse(counts$indicator_id == "sih_asthma", "SIH-RD", "SINAN-DENGUE")
  counts$numerator <- as.integer(seq_len(nrow(counts)) %% 4)
  counts$denominator <- ifelse(counts$year == 2025 & counts$age_group == "Total", 1000, NA_real_)
  counts$complete <- TRUE; counts$preliminary <- counts$year == 2026
  counts$measure <- "count"; counts$value <- as.numeric(counts$numerator)
  rates <- counts; rates$measure <- "rate"; rates$value <- rates$numerator / rates$denominator * 1e5
  health <- rbind(counts, rates)
  saveRDS(health, file.path(path, "health.rds"))
  saveRDS(data.frame(cod_mun = c(220005L, 220010L)), file.path(path, "geo.rds"))
  saveRDS(data.frame(), file.path(path, "population.rds"))
  saveRDS(list(), file.path(path, "climate_metadata.rds"))
  saveRDS(list(), file.path(path, "health_metadata.rds"))
  saveRDS(list(schema_version = 2L, sample = TRUE), file.path(path, "metadata.rds"))
  con <- DBI::dbConnect(duckdb::duckdb(), file.path(path, "terraclimate.duckdb"))
  DBI::dbWriteTable(con, "terraclimate", data.frame(cod_mun = 220005L, year = 2025L, month = 1L, name = "pdsi", value = 1))
  DBI::dbDisconnect(con, shutdown = TRUE)
  hashes <- setNames(vapply(file.path(path, code$RELEASE_FILES), code$file_sha256, ""), code$RELEASE_FILES)
  saveRDS(list(schema_version = 2L, sample = TRUE, health_rows = nrow(health), files = hashes), file.path(path, "validation.rds"))
  list(root = root, path = path, cache = file.path(root, "cache"), health = health)
}
f <- make_fixture()

tryCatch({
  test_that("preparation preserves fields, rates, zeros and missing values", {
    target <- code$prepare_health_cache(f$path, cache_dir = f$cache)
    d <- code$read_dashboard_data(f$path, f$cache)
    expect_false(is.null(d))
    actual <- code$query_health(d, "sih_asthma", 220005L, "Total", "rate")
    expected <- subset(f$health, indicator_id == "sih_asthma" & cod_mun == 220005L & age_group == "Total" & measure == "rate")
    expected <- expected[order(expected$year, expected$month), ]
    rownames(expected) <- NULL
    expect_equal(actual, expected)
    expect_true(all(is.na(actual$value[actual$year == 2026])))
    expect_true(all(actual$value[actual$year == 2025] == actual$numerator[actual$year == 2025] / actual$denominator[actual$year == 2025] * 1e5))
    counts <- code$query_health(d, "sih_asthma", 220005L, "Total", "count")
    expect_equal(counts$value, as.numeric(counts$numerator))
    expect_equal(nrow(code$query_health(d, "sih_asthma' OR TRUE --", 220005L, "Total", "count")), 0L)
    expect_error(DBI::dbExecute(d$health_con, "DELETE FROM health"), "read-only|read only", ignore.case = TRUE)
    code$close_dashboard_data(d)
    before <- file.info(target)$mtime
    expect_identical(code$prepare_health_cache(f$path, cache_dir = f$cache), target)
    expect_equal(file.info(target)$mtime, before)
  })

  test_that("startup does not deserialize the complete health table", {
    code$readRDS <- function(file, ...) {
      if (basename(file) == "health.rds") stop("Full health read during startup")
      base::readRDS(file, ...)
    }
    d <- code$read_dashboard_data(f$path, f$cache)
    expect_false(is.null(d))
    expect_false("health" %in% names(d))
    expect_gt(nrow(code$query_health(d, "sinan_dengue", 220010L, "Total", "count")), 0)
    code$close_dashboard_data(d)
    rm("readRDS", envir = code)
  })

  test_that("failed forced rebuild and abandoned files preserve the active generation", {
    release <- code$read_release(f$path)
    folder <- code$health_cache_dir(release, f$cache)
    pointer <- readRDS(file.path(folder, "current.rds"))
    validate <- code$validate_health_database
    code$validate_health_database <- function(...) stop("Interrupção simulada da preparação")
    expect_error(code$prepare_health_cache(f$path, force = TRUE, cache_dir = f$cache), "Interrupção simulada")
    code$validate_health_database <- validate
    expect_identical(readRDS(file.path(folder, "current.rds")), pointer)
    abandoned <- file.path(folder, "abandoned.partial.duckdb")
    writeLines("abandoned build", abandoned)
    d <- code$read_dashboard_data(f$path, f$cache)
    expect_false(is.null(d)); code$close_dashboard_data(d)
    next_target <- code$prepare_health_cache(f$path, force = TRUE, cache_dir = f$cache)
    expect_true(file.exists(file.path(folder, pointer$file)))
    expect_false(identical(basename(next_target), pointer$file))
  })

  test_that("corrupt and incompatible caches are refused and can be rebuilt", {
    release <- code$read_release(f$path)
    folder <- code$health_cache_dir(release, f$cache)
    pointer <- readRDS(file.path(folder, "current.rds"))
    writeLines("corrupted", file.path(folder, pointer$file))
    expect_message(expect_null(code$read_dashboard_data(f$path, f$cache)), "Dados indisponíveis")
    code$prepare_health_cache(f$path, cache_dir = f$cache)
    pointer <- readRDS(file.path(folder, "current.rds"))
    con <- DBI::dbConnect(duckdb::duckdb(), file.path(folder, pointer$file))
    DBI::dbExecute(con, "UPDATE cache_manifest SET cache_version = 999")
    DBI::dbDisconnect(con, shutdown = TRUE)
    expect_message(expect_null(code$read_dashboard_data(f$path, f$cache)), "incompatível")
    code$prepare_health_cache(f$path, cache_dir = f$cache)
  })

  test_that("a second process cannot prepare the same release concurrently", {
    folder <- code$health_cache_dir(code$read_release(f$path), f$cache)
    lock <- filelock::lock(file.path(folder, "prepare.lock"))
    script <- sprintf('source("R/data_access.R"); source("R/prepare_data.R"); tryCatch(prepare_health_cache(%s, cache_dir=%s), error=function(e) {message(conditionMessage(e)); quit(status=7)})',
      encodeString(f$path, quote = '"'), encodeString(f$cache, quote = '"'))
    child <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"), c("--vanilla", "-e", shQuote(script)), stdout = TRUE, stderr = TRUE))
    filelock::unlock(lock)
    expect_equal(attr(child, "status"), 7L)
    expect_match(paste(child, collapse = "\n"), "Preparação em andamento")
  })

  test_that("revised releases use a new cache and existing readers keep their release", {
    revised <- make_fixture()
    old <- code$read_dashboard_data(f$path, f$cache)
    revised$health$numerator <- revised$health$numerator + 10L
    revised$health$value <- ifelse(revised$health$measure == "count", revised$health$numerator,
      revised$health$numerator / revised$health$denominator * 1e5)
    saveRDS(revised$health, file.path(revised$path, "health.rds"))
    report <- readRDS(file.path(revised$path, "validation.rds"))
    report$files["health.rds"] <- code$file_sha256(file.path(revised$path, "health.rds"))
    saveRDS(report, file.path(revised$path, "validation.rds"))
    expect_false(identical(code$read_release(f$path)$fingerprint, code$read_release(revised$path)$fingerprint))
    expect_message(expect_null(code$read_dashboard_data(revised$path, f$cache)), "não preparado")
    code$prepare_health_cache(revised$path, cache_dir = f$cache)
    next_data <- code$read_dashboard_data(revised$path, f$cache)
    old_values <- code$query_health(old, "sih_asthma", 220005L, "Total", "count")$value
    next_values <- code$query_health(next_data, "sih_asthma", 220005L, "Total", "count")$value
    expect_equal(next_values, old_values + 10)
    code$close_dashboard_data(old); code$close_dashboard_data(next_data)
    unlink(revised$root, recursive = TRUE)
  })

  test_that("duplicate keys never become an active cache", {
    duplicate <- make_fixture()
    saveRDS(rbind(duplicate$health, duplicate$health[1, ]), file.path(duplicate$path, "health.rds"))
    report <- readRDS(file.path(duplicate$path, "validation.rds"))
    report$files["health.rds"] <- code$file_sha256(file.path(duplicate$path, "health.rds"))
    report$health_rows <- report$health_rows + 1L
    saveRDS(report, file.path(duplicate$path, "validation.rds"))
    expect_error(code$prepare_health_cache(duplicate$path, cache_dir = duplicate$cache), "duplicadas")
    expect_false(file.exists(file.path(code$health_cache_dir(code$read_release(duplicate$path), duplicate$cache), "current.rds")))
    unlink(duplicate$root, recursive = TRUE)
  })

  test_that("modified sources are rejected before replacing a prepared database", {
    writeLines("modified", file.path(f$path, "population.rds"))
    expect_error(code$prepare_health_cache(f$path, force = TRUE, cache_dir = f$cache), "modificados depois da validação")
  })

  test_that("missing months remain gaps", {
    x <- code$complete_months(data.frame(year = c(2024L, 2024L), month = c(1L, 3L), value = c(0, 2)))
    expect_equal(nrow(x), 3L); expect_true(is.na(x$value[2])); expect_equal(x$value[1], 0)
  })
}, finally = unlink(f$root, recursive = TRUE))
cat("Data access checks passed\n")
