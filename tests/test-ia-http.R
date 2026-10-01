# Real local HTTP requests catch timer/polling regressions that mocked replies miss.
# No external service or credentials. Requires callr.
suppressPackageStartupMessages(library(testthat))
source("R/ai.R")
port <- httpuv::randomPort()
server <- callr::r_bg(function(port) {
  httpuv::runServer("127.0.0.1", port, list(call = function(req) {
    if (req$PATH_INFO == "/slow") Sys.sleep(2)
    list(status = 200L, headers = list(`Content-Type` = "application/json"),
      body = '{"text_description":"Local test response"}')
  }))
}, args = list(port))
tryCatch({
  url <- sprintf("http://127.0.0.1:%d", port)
  up <- FALSE
  for (i in seq_len(100L)) {
    up <- tryCatch({
      curl::curl_fetch_memory(url, handle = curl::new_handle(timeout_ms = 200))
      TRUE
    }, error = function(e) FALSE)
    if (up) break
    Sys.sleep(0.05)
  }
  stopifnot(up)
  await_request <- function(path, timeout) {
    started <- proc.time()[["elapsed"]]
    done <- FALSE
    result <- NULL
    req <- httr2::request(paste0(url, path)) |> httr2::req_timeout(timeout)
    promises::then(perform_ai_request(req),
      onFulfilled = function(resp) { result <<- resp; done <<- TRUE },
      onRejected = function(error) { result <<- error; done <<- TRUE })
    while (!done && proc.time()[["elapsed"]] - started < 4) later::run_now(0.01)
    expect_true(done)
    list(result = result, elapsed = proc.time()[["elapsed"]] - started)
  }
  test_that("real HTTP responses complete asynchronously", {
    result <- await_request("/", 1)
    expect_s3_class(result$result, "httr2_response")
    expect_equal(httr2::resp_body_json(result$result)$text_description, "Local test response")
    expect_lt(result$elapsed, 1)
  })
  test_that("a stalled request expires before the server eventually answers", {
    result <- await_request("/slow", 0.25)
    expect_s3_class(result$result, "httr2_failure")
    expect_s3_class(result$result$parent, "curl_error_operation_timedout")
    expect_lt(result$elapsed, 1)
    expect_match(ai_request_error_message(result$result), "60 segundos")
  })
}, finally = server$kill())
