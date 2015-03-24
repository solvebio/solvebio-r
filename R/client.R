#' SolveBioClient
#'
#' Create an SolveBio client instance
#' @export
SolveBioClient <- function(api_key, api_host = "https://api.solvebio.com") {
  if (missing(api_key)) {
    stop("api_key parameter is required.")
  }

  url = httr::modify_url(api_host, "path" = "v1/user")
  authorization = paste("Token", api_key)
  headers <- httr::add_headers(
    Authorization = authorization,
    Accept = "application/json",
    "Content-Type" = "application/json",
    "Accept-Encoding" = "gzip,deflate",
    "User-Agent" = "SolveBio R Client"
  )
  res <- httr::GET(url, headers)
  httr::stop_for_status(res)

  if (res$status != 200) {
    stop()
  }
  res = formatSolveBioResult(res, raw = FALSE)

  structure(list("headers" = headers, "api_host" = api_host), class = "solvebio")
}
