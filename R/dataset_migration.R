#' DatasetMigration.all
#'
#' Retrieves the metadata about all dataset migrations on SolveBio.
#'
#' @param ... (optional) Additional query parameters (e.g. page).
#'
#' @examples \dontrun{
#' DatasetMigration.all()
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
DatasetMigration.all <- function(...) {
    .request('GET', "v1/dataset_migrations", query=list(...))
}

#' DatasetMigration.retrieve
#'
#' Retrieves the metadata about a specific dataset migration on SolveBio.
#'
#' @param id String The ID or full name of a SolveBio dataset migration.
#'
#' @examples \dontrun{
#' DatasetMigration.retrieve(<ID>)
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
DatasetMigration.retrieve <- function(id) {
    if (missing(id)) {
        stop("A dataset migration ID is required.")
    }

    path <- paste("v1/dataset_migrations", paste(id), sep="/")
    .request('GET', path=path)
}
