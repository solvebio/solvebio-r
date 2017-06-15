#' DatasetExport.all
#'
#' Retrieves the metadata about all dataset exports on SolveBio.
#'
#' @param ... (optional) Additional query parameters (e.g. page).
#'
#' @examples \dontrun{
#' DatasetExport.all()
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
DatasetExport.all <- function(...) {
    .request('GET', "v1/dataset_exports", query=list(...))
}

#' DatasetExport.retrieve
#'
#' Retrieves the metadata about a specific dataset export on SolveBio.
#'
#' @param id String The ID or full name of a SolveBio dataset export.
#'
#' @examples \dontrun{
#' DatasetExport.retrieve(<ID>)
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
DatasetExport.retrieve <- function(id) {
    if (missing(id)) {
        stop("A dataset export ID is required.")
    }

    path <- paste("v1/dataset_exports", paste(id), sep="/")
    .request('GET', path=path)
}
