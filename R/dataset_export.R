#' DatasetExport.all
#'
#' Retrieves the metadata about all dataset exports on SolveBio.
#'
#' @param env (optional) Custom client environment.
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
DatasetExport.all <- function(env = solvebio:::.solveEnv, ...) {
    .request('GET', "v2/dataset_exports", query=list(...), env=env)
}

#' DatasetExport.retrieve
#'
#' Retrieves the metadata about a specific dataset export on SolveBio.
#'
#' @param id String The ID of a SolveBio dataset export.
#' @param env (optional) Custom client environment.
#'
#' @examples \dontrun{
#' DatasetExport.retrieve(<ID>)
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
DatasetExport.retrieve <- function(id, env = solvebio:::.solveEnv) {
    if (missing(id)) {
        stop("A dataset export ID is required.")
    }

    path <- paste("v2/dataset_exports", paste(id), sep="/")
    .request('GET', path=path, env=env)
}


#' DatasetExport.delete
#'
#' Deletes a specific dataset export on SolveBio.
#'
#' @param id String The ID of a SolveBio dataset export.
#' @param env (optional) Custom client environment.
#'
#' @examples \dontrun{
#' DatasetExport.delete(<ID>)
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
DatasetExport.delete <- function(id, env = solvebio:::.solveEnv) {
    if (missing(id)) {
        stop("A dataset export ID is required.")
    }

    path <- paste("v2/dataset_exports", paste(id), sep="/")
    .request('DELETE', path=path, env=env)
}


#' DatasetExport.create
#'
#' Create a new dataset export.
#'
#' @param dataset_id The target dataset ID.
#' @param format (optional) The export format (default: json).
#' @param params (optional) Query parameters for the export.
#' @param env (optional) Custom client environment.
#' @param ... (optional) Additional dataset export parameters.
#'
#' @examples \dontrun{
#' DatasetExport.create(dataset_id=<ID>, format='json', params=list(fields=c("field_1"), limit=100))
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
DatasetExport.create <- function(
                                 dataset_id,
                                 format = 'json',
                                 params = list(),
                                 env = solvebio:::.solveEnv,
                                 ...) {
    if (missing(dataset_id)) {
        stop("A dataset ID is required.")
    }

    params = list(
                  dataset_id=dataset_id,
                  format=format,
                  params=params,
                  ...
                  )

    dataset_export <- .request('POST', path='v2/dataset_exports', query=NULL, body=params, env=env)

    return(dataset_export)
}


#' DatasetExport.get_download_url
#'
#' Helper method to get the download URL for a dataset export.
#'
#' @param id The ID of the dataset export.
#' @param env (optional) Custom client environment.
#'
#' @examples \dontrun{
#' DatasetExport.get_download_url("1234567890")
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
DatasetExport.get_download_url <- function(id, env = solvebio:::.solveEnv) {
    if (missing(id)) {
        stop("A dataset export ID is required.")
    }

    path <- paste("v2/dataset_exports", paste(id), "download", sep="/")
    response <- .request('GET', path=path, query=list(redirect=""), env=env)

    return(response$url)
}
