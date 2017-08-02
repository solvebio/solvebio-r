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
    .request('GET', "v2/dataset_imports", query=list(...))
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

    path <- paste("v2/dataset_imports", paste(id), sep="/")
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

    path <- paste("v2/dataset_imports", paste(id), sep="/")
    .request('DELETE', path=path)
}


#' DatasetImport.create
#'
#' Create a new dataset import. Either an object_id, manifest, or data_records is required.
#'
#' @param dataset_id The target dataset ID.
#' @param commit_mode (optional) The commit mode (default: append).
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
                                 ...) {
    if (missing(dataset_id)) {
        stop("A dataset ID is required.")
    }

    args = list(...)
    if (is.null(args$object_id) && is.null(args$manifest) && is.null(args$data_records)) {
        stop("Either an object, manifest, or data_records is required.")
    }

    params = list(
                  dataset_id=dataset_id,
                  commit_mode=commit_mode,
                  ...
                  )

    if (!is.null(args$object_id)) {
        # Create a manifest from the object
        object = Object.retrieve(args$object_id)
        if (is.null(object) || object$object_type != 'file') {
            stop("Invalid object: input object must be a file")
        }

        url = Object.get_download_url(object$id)
        params$manifest = list(
                               list(
                                    url=url,
                                    logical_object_id=object$id,
                                    name=object$filename
                                    )
                               )
    }

    dataset_import <- .request('POST', path='v2/dataset_imports', query=NULL, body=params)

    return(dataset_import)
}
