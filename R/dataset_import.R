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
#' @param id String The ID of a SolveBio dataset import.
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


#' DatasetImport.delete
#'
#' Deletes a specific dataset import on SolveBio.
#'
#' @param id String The ID of a SolveBio dataset import.
#'
#' @examples \dontrun{
#' DatasetImport.delete(<ID>)
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
DatasetImport.delete <- function(id) {
    if (missing(id)) {
        stop("A dataset import ID is required.")
    }

    path <- paste("v1/dataset_imports", paste(id), sep="/")
    .request('DELETE', path=path)
}


#' DatasetImport.create
#'
#' Create a new dataset import. Either an upload_id, manifest, or data_records is required.
#'
#' @param dataset_id The target dataset ID.
#' @param commit_mode (optional) The commit mode (default: append).
#' @param auto_approve (optional) Automatically approve the commit (default: TRUE).
#' @param ... (optional) Additional dataset import attributes.
#'
#' @examples \dontrun{
#' DatasetImport.create(dataset_id=<ID>, upload_id=<ID>)
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
DatasetImport.create <- function(
                                 dataset_id,
                                 commit_mode='append',
                                 auto_approve=TRUE,
                                 ...) {
    if (missing(dataset_id)) {
        stop("A dataset ID is required.")
    }

    params = list(
                  dataset_id=dataset_id,
                  commit_mode=commit_mode,
                  auto_approve=auto_approve,
                  ...
                  )

    dataset_import <- .request('POST', path='v1/dataset_imports', query=NULL, body=params)

    return(dataset_import)
}
