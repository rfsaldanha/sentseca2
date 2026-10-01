# The health RDS is read only here, never by the Shiny process.
atomic_cache_pointer <- function(value, path) {
  tmp <- tempfile("pointer-", tmpdir = dirname(path))
  on.exit(unlink(tmp))
  saveRDS(value, tmp)
  if (!file.rename(tmp, path)) stop("Não foi possível ativar o banco preparado")
}

validate_health_database <- function(con, release, sample) {
  rows <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM health")$n
  if (rows != release$report$health_rows) stop("Quantidade de registros de saúde divergente")
  duplicates <- DBI::dbGetQuery(con, paste("SELECT 1 FROM health GROUP BY",
    paste(HEALTH_KEY, collapse = ","), "HAVING COUNT(*) > 1 LIMIT 1"))
  if (nrow(duplicates)) stop("Chaves de saúde duplicadas")
  invalid <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM health WHERE
    numerator IS NULL OR numerator < 0 OR numerator <> FLOOR(numerator)
    OR month NOT BETWEEN 1 AND 12 OR month IS NULL OR year IS NULL OR cod_mun IS NULL
    OR indicator_id IS NULL OR age_group IS NULL OR measure IS NULL
    OR complete IS NULL OR preliminary IS NULL OR source IS NULL
    OR measure NOT IN ('count', 'rate')
    OR (measure = 'count' AND value IS DISTINCT FROM numerator)
    OR (measure = 'rate' AND CASE WHEN denominator > 0 AND complete AND age_group <> 'Idade ignorada'
      THEN value IS NULL OR ABS(value - numerator / denominator * 100000) > 0.00000001
      ELSE value IS NOT NULL END)")$n
  if (invalid > 0) stop("Contagens, taxas ou chaves de saúde inválidas")
  duckdb::duckdb_register(con, "source_sample", sample)
  on.exit(duckdb::duckdb_unregister(con, "source_sample"))
  actual <- DBI::dbGetQuery(con, paste0("SELECT h.* FROM health h INNER JOIN source_sample s USING (",
    paste(HEALTH_KEY, collapse = ","), ")"))
  sort_rows <- function(x) {
    x <- as.data.frame(x)
    x <- x[do.call(order, x[HEALTH_KEY]), , drop = FALSE]
    rownames(x) <- NULL
    x
  }
  if (!isTRUE(all.equal(sort_rows(actual), sort_rows(sample), check.attributes = FALSE, tolerance = 1e-10)))
    stop("Amostra do banco difere dos dados originais")
  invisible(rows)
}

prepare_health_cache <- function(base = Sys.getenv("SENTSECA_DATA_DIR", "data"), force = FALSE,
                                 cache_dir = Sys.getenv("SENTSECA_CACHE_DIR", "data/cache")) {
  release <- read_release(base)
  folder <- health_cache_dir(release, cache_dir)
  dir.create(folder, recursive = TRUE, showWarnings = FALSE)
  lock <- filelock::lock(file.path(folder, "prepare.lock"), timeout = 0)
  if (is.null(lock)) stop("Preparação em andamento para esta publicação; tente novamente após sua conclusão")
  on.exit(filelock::unlock(lock), add = TRUE)
  message("Conferindo os arquivos da publicação ", basename(release$path), "...")
  verify_release(release)
  if (!force) {
    existing <- tryCatch(open_health_cache(release, cache_dir, verify_checksum = TRUE), error = function(e) NULL)
    if (!is.null(existing)) {
      DBI::dbDisconnect(existing$con, shutdown = TRUE)
      message("Banco íntegro reutilizado: ", existing$path)
      return(invisible(existing$path))
    }
  }
  # Each build gets its own files. Neither failures nor --force overwrite a live generation.
  stage <- tempfile("build-", tmpdir = folder, fileext = ".partial.duckdb")
  scratch <- paste0(stage, ".tmp")
  con <- NULL
  on.exit({
    if (!is.null(con) && DBI::dbIsValid(con)) DBI::dbDisconnect(con, shutdown = TRUE)
    unlink(c(stage, paste0(stage, ".wal"), scratch), recursive = TRUE)
  }, add = TRUE, after = FALSE)
  message("Lendo health.rds para a conversão única desta publicação...")
  health <- readRDS(file.path(release$path, "health.rds"))
  if (!all(HEALTH_COLUMNS %in% names(health)) || nrow(health) != release$report$health_rows)
    stop("Estrutura ou quantidade de registros de saúde incompatível")
  indices <- unique(as.integer(round(seq(1, nrow(health), length.out = min(1000L, nrow(health))))))
  sample <- health[indices, , drop = FALSE]
  con <- DBI::dbConnect(duckdb::duckdb(), stage, config = list(memory_limit = "4GB", threads = "2", temp_directory = scratch))
  duckdb::duckdb_register(con, "source_health", health)
  message("Gravando ", format(nrow(health), big.mark = ".", decimal.mark = ","), " registros em DuckDB...")
  DBI::dbExecute(con, paste("CREATE TABLE health AS SELECT * FROM source_health ORDER BY", paste(HEALTH_KEY, collapse = ",")))
  duckdb::duckdb_unregister(con, "source_health")
  rm(health); invisible(gc())
  message("Validando chaves, taxas e equivalência com a publicação...")
  rows <- validate_health_database(con, release, sample)
  DBI::dbExecute(con, "CREATE TABLE health_availability AS SELECT indicator_id, age_group, measure,
    MIN(make_date(year, month, 1)) AS first_date, MAX(make_date(year, month, 1)) AS last_date,
    COUNT(*) AS rows, COUNT(value) AS available_values FROM health GROUP BY indicator_id, age_group, measure")
  DBI::dbWriteTable(con, "cache_manifest", data.frame(cache_version = HEALTH_CACHE_VERSION,
    fingerprint = release$fingerprint, health_sha256 = unname(release$report$files["health.rds"]),
    health_rows = as.numeric(rows), created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")))
  DBI::dbExecute(con, "CHECKPOINT")
  DBI::dbDisconnect(con, shutdown = TRUE); con <- NULL
  target <- file.path(folder, paste0("health-", sub("\\.partial\\.duckdb$", "", basename(stage)), ".duckdb"))
  if (!file.rename(stage, target)) stop("Não foi possível finalizar o banco de saúde")
  # Pointer changes last; previous generations remain available to running processes and rollback.
  atomic_cache_pointer(list(file = basename(target), sha256 = file_sha256(target)), file.path(folder, "current.rds"))
  message("Banco validado e preparado: ", target)
  invisible(target)
}
