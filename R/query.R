#' query
#'
#' Returns filtered documents from a SolveBio dataset.
#' \code{query} executes a SolveBio dataset query and retrieves the results.
#' @param client SolveBioClient A SolveBioClient object
#' @param dataset String The ID or full name of a SolveBio dataset
#' @param filters Json
#' @param params Json Additional query parameters
#'
#' @examples
#' sb = SolveBioClient(Sys.getenv("SOLVEBIO_API_KEY"))
#' query(sb, dataset="ClinVar/ClinVar")
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
query <- function (client, dataset, filters, params) {
  UseMethod("query", client)
}

#' @rdname query
#' @export
query.solvebio <- function (client, dataset, filters = NULL, params = NULL) {
  # args = prepareArgs(args)
  path = paste("/v1/datasets", paste(dataset), "data", sep="/")
  url = httr::modify_url(client$api_host, "path" = path)
  if (!missing(filters)) {
    res = httr::POST(url, client$headers, body = filters, encode = "json")
  } else {
    res = httr::POST(url, client$headers)
  }
  httr::stop_for_status(res)
  formatSolveBioResult(res)
}
