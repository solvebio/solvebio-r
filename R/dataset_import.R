#' DatasetImport.all
#'
#' Retrieves the metadata about all dataset imports on SolveBio.
#'
#' @param ... (optional) Additional query parameters (e.g. page).
#'
#' @examples \dontrun{
#' DatasetImport.all()
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
DatasetImport.all <- function(...) {
    .request('GET', "v1/dataset_imports", query=list(...))
}

#' DatasetImport.retrieve
#'
#' Retrieves the metadata about a specific dataset import on SolveBio.
#'
#' @param id String The ID or full name of a SolveBio dataset import.
#'
#' @examples \dontrun{
#' DatasetImport.retrieve(<ID>)
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
DatasetImport.retrieve <- function(id) {
    if (missing(id)) {
        stop("A dataset import ID is required.")
    }

    path <- paste("v1/dataset_imports", paste(id), sep="/")
    .request('GET', path=path)
}
