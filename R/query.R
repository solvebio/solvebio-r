#' query
#'
#' Returns filtered documents from a SolveBio dataset.
#' \code{query} executes a SolveBio dataset query and retrieves the results.
#' @param client SolveBioClient A SolveBioClient object
#' @param dataset String The ID or full name of a SolveBio dataset
#' @param filters Json
#' @param params Json Additional query parameters
#'
#' @examples
#' client <- SolveBioClient()
#' query(client, dataset="ClinVar/ClinVar")
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
query <- function (client, dataset, filters = NULL, params = NULL) {
    path <- paste("/v1/datasets", paste(dataset), "data", sep="/")

    res <- client$request(
                          'POST',
                          path = path,
                          body = NULL)
    return(res)
}
