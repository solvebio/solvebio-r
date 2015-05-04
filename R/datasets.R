#' Dataset.all
#'
#' Retrieves the metadata about all datasets on SolveBio.
#'
#' @examples \dontrun{
#' Dataset.all()
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
Dataset.all <- function(params) {
    .request('GET', '/v1/datasets', )
}

#' Dataset.retrieve
#'
#' Retrieves the metadata about a specific dataset from SolveBio.
#'
#' @param id String The ID or full name of a SolveBio dataset
#'
#' @examples \dontrun{
#' Dataset.retrieve("ClinVar/ClinVar")
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
Dataset.retrieve <- function(id, ...) {
    if (missing(id)) {
        stop("A dataset ID or name is required.")
    }

    path <- paste("/v1/datasets", paste(id), sep="/")
    .request('GET', path=path)
}

#' Dataset.query
#'
#' Returns filtered documents from a SolveBio dataset.
#' \code{query} executes a SolveBio dataset query and retrieves the results.
#' @param id String The ID or full name of a SolveBio dataset
#' @param filters Json
#' @param params Json Additional query parameters
#'
#' @examples \dontrun{
#' login()
#' Dataset.query("ClinVar/ClinVar")
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
Dataset.query <- function(id, filters = NULL, params = list()) {
    if (missing(id)) {
        stop("A dataset ID or name is required.")
    }

    path <- paste("/v1/datasets", paste(id), "data", sep="/")
    .request('POST', path=path, body=filters, query=params)
}
