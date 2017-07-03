#' DatasetCommit.all
#'
#' Retrieves the metadata about all dataset commits on SolveBio.
#'
#' @param ... (optional) Additional query parameters (e.g. page).
#'
#' @examples \dontrun{
#' DatasetCommit.all()
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
DatasetCommit.all <- function(...) {
    .request('GET', "v1/dataset_commits", query=list(...))
}


#' DatasetCommit.retrieve
#'
#' Retrieves the metadata about a specific dataset commit on SolveBio.
#'
#' @param id String The ID of a SolveBio dataset commit.
#'
#' @examples \dontrun{
#' DatasetCommit.retrieve(<ID>)
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
DatasetCommit.retrieve <- function(id) {
    if (missing(id)) {
        stop("A dataset commit ID is required.")
    }

    path <- paste("v1/dataset_commits", paste(id), sep="/")
    .request('GET', path=path)
}


#' DatasetCommit.delete
#'
#' Deletes a specific dataset commit on SolveBio.
#'
#' @param id String The ID or full name of a SolveBio dataset commit.
#'
#' @examples \dontrun{
#' DatasetCommit.delete(<ID>)
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
DatasetCommit.delete <- function(id) {
    if (missing(id)) {
        stop("A dataset commit ID is required.")
    }

    path <- paste("v1/dataset_commits", paste(id), sep="/")
    .request('DELETE', path=path)
}
