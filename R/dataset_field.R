#' DatasetField.all
#'
#' Retrieves the metadata about all dataset fields on SolveBio.
#'
#' @param ... (optional) Additional query parameters (e.g. page).
#'
#' @examples \dontrun{
#' DatasetField.all()
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
DatasetField.all <- function(...) {
    .request('GET', "v1/dataset_fields", query=list(...))
}

#' DatasetField.retrieve
#'
#' Retrieves the metadata about a specific dataset field.
#'
#' @param id String The ID or full name of a dataset field.
#'
#' @examples \dontrun{
#' DatasetField.retrieve(691)
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
DatasetField.retrieve <- function(id) {
    if (missing(id)) {
        stop("A dataset field ID or name is required.")
    }

    path <- paste("v1/dataset_fields", paste(id), sep="/")
    .request('GET', path=path)
}

#' DatasetField.facets
#'
#' Returns the facets for a given dataset field.
#'
#' @param id String The ID or full name of a dataset field.
#' @param ... (optional) Additional query parameters.
#'
#' @examples \dontrun{
#' DatasetField.facets(691)
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
DatasetField.facets <- function(id, ...) {
    if (missing(id) | !(class(id) %in% c("DatasetField", "numeric", "character"))) {
        stop("A dataset field ID or name is required.")
    }
    if (class(id) == "DatasetField") {
        id <- id$id
    }

    path <- paste("v1/dataset_fields", paste(id), "facets", sep="/")
    .request('GET', path=path, query=list(...))
}
