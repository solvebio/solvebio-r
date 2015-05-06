#' Dataset.all
#'
#' Retrieves the metadata about all datasets on SolveBio.
#'
#' @examples \dontrun{
#' Dataset.all()
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
Dataset.all <- function(...) {
    .request('GET', "v1/datasets", query=list(...))
}

#' Dataset.retrieve
#'
#' Retrieves the metadata about a specific dataset from SolveBio.
#'
#' @param id String The ID or full name of a SolveBio dataset
#'
#' @examples \dontrun{
#' Dataset.retrieve("ClinVar/ClinVar")
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
Dataset.retrieve <- function(id) {
    if (missing(id)) {
        stop("A dataset ID or name is required.")
    }

    path <- paste("v1/datasets", paste(id), sep="/")
    .request('GET', path=path)
}

#' Dataset.query
#'
#' Returns filtered documents from a SolveBio dataset.
#' \code{query} executes a SolveBio dataset query and retrieves the results.
#' @param id String The ID or full name of a SolveBio dataset, or a Dataset object.
#' @param filters Json (optional) Query filters.
#' @param params Json (optional) Query parameters.
#'
#' @examples \dontrun{
#' login()
#' Dataset.query("ClinVar/ClinVar")
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
Dataset.query <- function(id, filters, ...) {
    if (missing(id) | !(class(id) %in% c("Dataset", "numeric", "character"))) {
        stop("A dataset ID or name is required.")
    }
    if (class(id) == "Dataset") {
        id <- id$id
    }

    body = list(...)

    # Filters can be passed as a JSON string
    if (!missing(filters)) {
        if (class(filters) == "character") {
            # Convert JSON string to an R structure
            filters <- jsonlite::fromJSON(filters)
        }
        # Add filters to body
        body = modifyList(body, list(filters=filters))
    }

    path <- paste("v1/datasets", paste(id), "data", sep="/")
    .request('POST', path=path, body=body)
}

# TODO: pretty-print Dataset objects
# print.Dataset <- function(x, ...) {
#     cat(sprintf("Dataset: %s", x$full_name))
# }
