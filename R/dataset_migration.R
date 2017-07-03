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
#' @param id String The ID of a SolveBio dataset migration.
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


#' DatasetMigration.delete
#'
#' Deletes specific dataset migration on SolveBio.
#'
#' @param id String The ID of a SolveBio dataset migration.
#'
#' @examples \dontrun{
#' DatasetMigration.delete(<ID>)
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
DatasetMigration.delete <- function(id) {
    if (missing(id)) {
        stop("A dataset migration ID is required.")
    }

    path <- paste("v1/dataset_migrations", paste(id), sep="/")
    .request('DELETE', path=path)
}


#' DatasetMigration.create
#'
#' Create a new dataset migration.
#'
#' @param source_id The source dataset ID.
#' @param target_id The target dataset ID.
#' @param commit_mode (optional) The commit mode (default: append).
#' @param source_params (optional) The query parameters used on the source dataset.
#' @param target_fields (optional) A list of valid dataset fields to add or override in the target dataset.
#' @param auto_approve (optional) Set to TRUE to automatically approve (and start) the resulting commit (default: TRUE).
#' @param include_errors (optional) If TRUE, a new field (_errors) will be added to each record containing expression evaluation errors (default: FALSE).
#' @param ... (optional) Additional dataset migration attributes.
#'
#' @examples \dontrun{
#' DatasetMigration.create(dataset_id=<ID>, upload_id=<ID>)
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
DatasetMigration.create <- function(
                                    source_id,
                                    target_id,
                                    commit_mode='append',
                                    source_params=NULL,
                                    target_fields=NULL,
                                    auto_approve=TRUE,
                                    include_errors=FALSE,
                                    ...) {
    if (missing(source_id)) {
        stop("A source dataset ID is required.")
    }

    if (missing(target_id)) {
        stop("A target dataset ID is required.")
    }

    params = list(
                  source_id=source_id,
                  target_id=target_id,
                  source_params=source_params,
                  target_fields=target_fields,
                  commit_mode=commit_mode,
                  auto_approve=auto_approve,
                  include_errors=include_errors,
                  ...
                  )

    dataset_migration <- .request('POST', path='v1/dataset_migrations', query=NULL, body=params)

    return(dataset_migration)
}
