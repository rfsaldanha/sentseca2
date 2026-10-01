# Published releases are immutable. A running process keeps one release until restart.
HEALTH_CACHE_VERSION <- 1L
HEALTH_KEY <- c("indicator_id", "cod_mun", "age_group", "measure", "year", "month")
HEALTH_COLUMNS <- c(HEALTH_KEY, "source", "value", "numerator", "denominator", "complete", "preliminary")
RELEASE_FILES <- c("geo.rds", "terraclimate.duckdb", "health.rds", "population.rds",
                   "climate_metadata.rds", "health_metadata.rds", "metadata.rds")

resolve_data_dir <- function(base = Sys.getenv("SENTSECA_DATA_DIR", "data")) {
  pointer <- file.path(base, "current.rds")
  if (file.exists(pointer)) {
    relative <- readRDS(pointer)
    if (!is.character(relative) || length(relative) != 1L || is.na(relative) ||
        !grepl("^releases/[A-Za-z0-9_-]+$", relative)) stop("Ponteiro de dados inválido")
    base <- file.path(base, relative)
  }
  normalizePath(base, mustWork = TRUE)
}

file_sha256 <- function(path) digest::digest(file = path, algo = "sha256")

read_release <- function(base = Sys.getenv("SENTSECA_DATA_DIR", "data")) {
  path <- resolve_data_dir(base)
  if (!all(file.exists(file.path(path, c(RELEASE_FILES, "validation.rds")))))
    stop("Publicação incompleta ou sem relatório de validação")
  report <- readRDS(file.path(path, "validation.rds"))
  metadata <- readRDS(file.path(path, "metadata.rds"))
  if (!identical(metadata$schema_version, 2L) || !identical(report$schema_version, 2L) ||
      !identical(metadata$sample, report$sample)) stop("Versão de dados incompatível")
  if (!is.character(report$files) || !setequal(names(report$files), RELEASE_FILES) ||
      anyNA(report$files) || any(!grepl("^[a-f0-9]{64}$", report$files)) ||
      length(report$health_rows) != 1L || is.na(report$health_rows) || report$health_rows < 1)
    stop("Relatório de validação inválido")
  fingerprint <- digest::digest(paste(c(HEALTH_CACHE_VERSION,
    paste(RELEASE_FILES, report$files[RELEASE_FILES], sep = "=")), collapse = "\n"),
    algo = "sha256", serialize = FALSE)
  list(path = path, metadata = metadata, report = report, fingerprint = fingerprint)
}

verify_release <- function(release) {
  actual <- vapply(file.path(release$path, RELEASE_FILES), file_sha256, "")
  if (!identical(unname(actual), unname(release$report$files[RELEASE_FILES])))
    stop("Arquivos modificados depois da validação; preparação cancelada")
  invisible(release)
}

health_cache_dir <- function(release, cache_dir = Sys.getenv("SENTSECA_CACHE_DIR", "data/cache")) {
  file.path(cache_dir, paste0("health-v", HEALTH_CACHE_VERSION, "-", release$fingerprint))
}

open_health_cache <- function(release, cache_dir = Sys.getenv("SENTSECA_CACHE_DIR", "data/cache"),
                              verify_checksum = FALSE) {
  folder <- health_cache_dir(release, cache_dir)
  pointer_file <- file.path(folder, "current.rds")
  if (!file.exists(pointer_file)) stop("Banco de saúde ainda não preparado para esta publicação")
  pointer <- readRDS(pointer_file)
  if (!is.list(pointer) || !is.character(pointer$file) || length(pointer$file) != 1L ||
      is.na(pointer$file) || !grepl("^health-[A-Za-z0-9_-]+\\.duckdb$", pointer$file) ||
      !is.character(pointer$sha256) || length(pointer$sha256) != 1L || is.na(pointer$sha256))
    stop("Ponteiro do banco de saúde inválido")
  path <- file.path(folder, pointer$file)
  if (verify_checksum && !identical(file_sha256(path), pointer$sha256))
    stop("Checksum do banco de saúde inválido")
  con <- DBI::dbConnect(duckdb::duckdb(), path, read_only = TRUE,
                       config = list(memory_limit = "512MB", threads = "2"))
  keep <- FALSE
  on.exit(if (!keep) DBI::dbDisconnect(con, shutdown = TRUE))
  if (!all(c("health", "cache_manifest", "health_availability") %in% DBI::dbListTables(con)))
    stop("Banco de saúde incompleto")
  manifest <- DBI::dbReadTable(con, "cache_manifest")
  if (nrow(manifest) != 1L || !identical(manifest$fingerprint, release$fingerprint) ||
      !identical(manifest$cache_version, HEALTH_CACHE_VERSION) ||
      !identical(manifest$health_sha256, unname(release$report$files["health.rds"])) ||
      manifest$health_rows != release$report$health_rows ||
      !all(HEALTH_COLUMNS %in% DBI::dbListFields(con, "health")) ||
      DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM health")$n != release$report$health_rows)
    stop("Banco de saúde incompatível com a publicação")
  availability <- DBI::dbReadTable(con, "health_availability")
  if (!nrow(availability) || !all(c("indicator_id", "age_group", "measure", "first_date", "last_date") %in% names(availability)))
    stop("Catálogo de saúde inválido")
  keep <- TRUE
  list(con = con, path = path, availability = availability, manifest = manifest)
}

close_dashboard_data <- function(dataset) {
  if (is.null(dataset)) return(invisible(NULL))
  for (con in list(dataset$health_con, dataset$climate_con))
    if (!is.null(con) && DBI::dbIsValid(con)) DBI::dbDisconnect(con, shutdown = TRUE)
  invisible(NULL)
}

read_dashboard_data <- function(base = Sys.getenv("SENTSECA_DATA_DIR", "data"),
                                cache_dir = Sys.getenv("SENTSECA_CACHE_DIR", "data/cache")) {
  dataset <- NULL
  tryCatch({
    release <- read_release(base)
    health <- open_health_cache(release, cache_dir)
    dataset <- list(path = release$path, health_con = health$con, health_cache_path = health$path,
                    availability = health$availability, metadata = release$metadata)
    dataset$climate_con <- DBI::dbConnect(duckdb::duckdb(), file.path(release$path, "terraclimate.duckdb"),
      read_only = TRUE, config = list(memory_limit = "512MB", threads = "2"))
    dataset$geo <- readRDS(file.path(release$path, "geo.rds"))
    dataset$climate <- readRDS(file.path(release$path, "climate_metadata.rds"))
    dataset$health_metadata <- readRDS(file.path(release$path, "health_metadata.rds"))
    dataset
  }, error = function(e) {
    close_dashboard_data(dataset)
    message("Dados indisponíveis: ", conditionMessage(e),
            ". Execute Rscript scripts/prepare_data.R com o mesmo SENTSECA_DATA_DIR e SENTSECA_CACHE_DIR, e reinicie o painel.")
    NULL
  })
}

query_health <- function(dataset, indicator_id, cod_mun, age_group, measure) {
  DBI::dbGetQuery(dataset$health_con,
    "SELECT * FROM health WHERE indicator_id = ? AND cod_mun = ? AND age_group = ? AND measure = ? ORDER BY year, month",
    params = list(indicator_id, as.integer(cod_mun), age_group, measure))
}

complete_months <- function(data, start = NULL, end = NULL) {
  if (!nrow(data)) return(data)
  data$date <- as.Date(sprintf("%04d-%02d-01", data$year, data$month))
  if (is.null(start)) start <- min(data$date)
  if (is.null(end)) end <- max(data$date)
  merge(data.frame(date = seq(start, end, by = "month")), data, by = "date", all.x = TRUE, sort = TRUE)
}
