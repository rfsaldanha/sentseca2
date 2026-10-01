# Run with /usr/bin/time -v Rscript tests/benchmark.R to also measure peak RSS.
env <- new.env()
reads <- character()
env$readRDS <- function(file, ...) {
  if (basename(file) == "health.rds") stop("Unexpected full health deserialization during startup")
  reads <<- c(reads, basename(file))
  base::readRDS(file, ...)
}
load_time <- system.time(sys.source("app.R", env))
stopifnot(!is.null(env$dataset))
tryCatch({
  d <- env$dataset
  query_time <- system.time(x <- env$query_health(d, d$availability$indicator_id[1], d$geo$cod_mun[1], "Total", "count"))
  cat(sprintf("Release: %s\nStartup: %.3f s\nHealth query: %.3f s\nReturned rows: %d\nDataset R object: %.2f MiB\n",
    basename(d$path), load_time[["elapsed"]], query_time[["elapsed"]], nrow(x), as.numeric(object.size(d)) / 1024^2))
  cat("RDS files read:", paste(unique(reads), collapse = ", "), "\n")
  stopifnot(!"health.rds" %in% reads, nrow(x) > 0)
}, finally = env$close_dashboard_data(env$dataset))
