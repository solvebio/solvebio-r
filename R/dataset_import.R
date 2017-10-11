#' DatasetImport.all
#'
#' Retrieves the metadata about all dataset imports on SolveBio.
#'
#' @param env (optional) Custom client environment.
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
DatasetImport.all <- function(env = solvebio:::.solveEnv, ...) {
    .request('GET', "v2/dataset_imports", query=list(...), env=env)
}


#' DatasetImport.retrieve
#'
#' Retrieves the metadata about a specific dataset import on SolveBio.
#'
#' @param id String The ID of a SolveBio dataset import.
#' @param env (optional) Custom client environment.
#'
#' @examples \dontrun{
#' DatasetImport.retrieve(<ID>)
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
DatasetImport.retrieve <- function(id, env = solvebio:::.solveEnv) {
    if (missing(id)) {
        stop("A dataset import ID is required.")
    }

    path <- paste("v2/dataset_imports", paste(id), sep="/")
    .request('GET', path=path, env=env)
}


#' DatasetImport.delete
#'
#' Deletes a specific dataset import on SolveBio.
#'
#' @param id String The ID of a SolveBio dataset import.
#' @param env (optional) Custom client environment.
#'
#' @examples \dontrun{
#' DatasetImport.delete(<ID>)
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
DatasetImport.delete <- function(id, env = solvebio:::.solveEnv) {
    if (missing(id)) {
        stop("A dataset import ID is required.")
    }

    path <- paste("v2/dataset_imports", paste(id), sep="/")
    .request('DELETE', path=path, env=env)
}


#' DatasetImport.create
#'
#' Create a new dataset import. Either an object_id, manifest, or data_records is required.
#'
#' @param dataset_id The target dataset ID.
#' @param commit_mode (optional) The commit mode (default: append).
#' @param env (optional) Custom client environment.
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
                                 commit_mode = 'append',
                                 env = solvebio:::.solveEnv,
                                 ...) {
    if (missing(dataset_id)) {
        stop("A dataset ID is required.")
    }

    params = list(
                  dataset_id=dataset_id,
                  commit_mode=commit_mode,
                  ...
                  )

    dataset_import <- .request('POST', path='v2/dataset_imports', query=NULL, body=params, env=env)

    return(dataset_import)
}
