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


#' DatasetImport.create
#'
#' Create a new dataset import.
#'
#' @param dataset_id The target dataset ID.
#' @param upload_id An upload ID
#' @param manifest (optional) A valid file manifest (alternative to upload_id).
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
                                 upload_id,
                                 manifest=NULL,
                                 commit_mode='append',
                                 auto_approve=TRUE,
                                 ...) {
    if (missing(dataset_id)) {
        stop("A dataset ID is required.")
    }

    if (missing(upload_id)) {
        if (is.null(manifest)) {
            stop("Either an upload ID or manifest is required.")
        }
        upload_id = NULL
    }

    params = list(
                  dataset_id=dataset_id,
                  upload_id=upload_id,
                  manifest=manifest,
                  commit_mode=commit_mode,
                  auto_approve=auto_approve,
                  ...
                  )

    dataset_import <- .request('POST', path='v1/dataset_imports', query=NULL, body=params)

    return(dataset_import)
}
