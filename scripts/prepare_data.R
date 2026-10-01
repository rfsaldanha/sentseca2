#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly = TRUE)
entry <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1])
root <- dirname(dirname(normalizePath(entry)))
source(file.path(root, "R", "data_access.R"))
source(file.path(root, "R", "prepare_data.R"))
main <- function(args) {
  if ("--help" %in% args) {
    cat("Uso: Rscript scripts/prepare_data.R [--data-dir DIRETORIO] [--force]\n",
        "SENTSECA_DATA_DIR: publicação ou diretório com current.rds (padrão: data).\n",
        "SENTSECA_CACHE_DIR: bancos derivados (padrão: data/cache).\n", sep = "")
    return(invisible(NULL))
  }
  base <- Sys.getenv("SENTSECA_DATA_DIR", file.path(root, "data"))
  cache <- Sys.getenv("SENTSECA_CACHE_DIR", file.path(root, "data", "cache"))
  force <- FALSE
  while (length(args)) {
    if (args[1] == "--force") { force <- TRUE; args <- args[-1]
    } else if (args[1] == "--data-dir" && length(args) >= 2L && !startsWith(args[2], "--")) {
      base <- args[2]; args <- args[-c(1, 2)]
    } else stop("Argumento inválido: ", args[1], ". Consulte --help.")
  }
  prepare_health_cache(base, force, cache)
}
tryCatch(main(args), error = function(e) { message("Falha na preparação: ", conditionMessage(e)); quit(status = 1L) })
