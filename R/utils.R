#' @export
formatSolveBioResult <- function (res, raw = FALSE) {
  url = res$url
  body = httr::content(res, as="text")
  if (raw) {
    body
  } else {
    res = jsonlite::fromJSON(body)
    res$'_url' = url
    res
  }
}

#' @export
prepareArgs <- function (args) {
  if (!is.null(args) && length(args) > 0) {
    args = args[!sapply(args, is.null)]
    rapply(args, function (x) {
      ifelse(is.logical(x), x * 1, x)
    }, how="replace")
  } else {
    NULL
  }
}
