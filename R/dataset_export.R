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


#' DatasetExport.create
#'
#' Create a new dataset export.
#'
#' @param dataset_id The target dataset ID.
#' @param format (optional) The export format (default: json).
#' @param params (optional) Query parameters for the export.
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
                                 format='json',
                                 params=list(),
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

    dataset_export <- .request('POST', path='v1/dataset_exports', query=NULL, body=params)

    return(dataset_export)
}
