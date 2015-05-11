#' DepositoryVersion.all
#'
#' Retrieves the metadata about all depository versions on SolveBio.
#'
#' @param ... (optional) Additional query parameters (e.g. page).
#'
#' @examples \dontrun{
#' DepositoryVersion.all()
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
DepositoryVersion.all <- function(...) {
    .request('GET', "v1/depository_versions", query=list(...))
}

#' DepositoryVersion.retrieve
#'
#' Retrieves the metadata about a specific depository version.
#'
#' @param id String The ID or full name of a depository version
#'
#' @examples \dontrun{
#' DepositoryVersion.retrieve("ClinVar/1.0.0")
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
DepositoryVersion.retrieve <- function(id) {
    if (missing(id)) {
        stop("A depository version ID or name is required.")
    }

    path <- paste("v1/depository_versions", paste(id), sep="/")
    .request('GET', path=path)
}

#' DepositoryVersion.datasets
#'
#' Returns the list of datasets for a given depository version.
#'
#' @param id String The ID or full name of a depository version.
#' @param ... (optional) Additional query parameters (e.g. page).
#'
#' @examples \dontrun{
#' DepositoryVersion.datasets("ClinVar/1.0.0")
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
DepositoryVersion.datasets <- function(id, ...) {
    if (missing(id) | !(class(id) %in% c("DepositoryVersion", "numeric", "character"))) {
        stop("A depository version ID or name is required.")
    }
    if (class(id) == "DepositoryVersion") {
        id <- id$id
    }

    path <- paste("v1/depository_versions", paste(id), "datasets", sep="/")
    .request('GET', path=path, query=list(...))
}
