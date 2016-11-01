#' Dataset.all
#'
#' Retrieves the metadata about all datasets on SolveBio.
#'
#' @param ... (optional) Additional query parameters (e.g. page).
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

#' Dataset.data
#'
#' Returns one page of documents from a SolveBio dataset.
#' \code{query} executes a request for a page of data and processes the response.
#' @param id The ID or full name of a SolveBio dataset, or a Dataset object.
#' @param filters (optional) Query filters.
#' @param ... (optional) Additional query parameters (e.g. limit, offset).
#'
#' @examples \dontrun{
#' login()
#' Dataset.data("ClinVar/ClinVar")
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
Dataset.data <- function(id, filters, ...) {
    if (missing(id) | !(class(id) %in% c("Dataset", "numeric", "character"))) {
        stop("A dataset ID or name is required.")
    }
    if (class(id) == "Dataset") {
        id <- id$id
    }

    params = list(...)

    # Filters can be passed as a JSON string
    if (!missing(filters)) {
        if (class(filters) == "character") {
            # Convert JSON string to an R structure
            filters <- jsonlite::fromJSON(filters)
        }
        # Add filters to params
        params = modifyList(params, list(filters=filters))
    }

    path <- paste("v1/datasets", paste(id), "data", sep="/")

    tryCatch({
        res <- .request('POST', path=path, body=params)
        return(formatSolveBioQueryResponse(res))
    }, error = function(e) {
        cat(sprintf("Query failed: %s\n", e$message))
    })
}


Dataset.query <- function(id, paginate=TRUE, ...) {
    # Max allowed records (total) when paginating
    max_records = 1000000
    params <- list(...)
    filters <- a$filters
    # Remove filters from params since it is an explicit param of Dataset.data()
    params['filters'] <- NULL

    # Retrieve the first page of results
    response <- do.call(Dataset.data, c(id=id, filters, params))
    df <- response$result
    offset <- response$offset

    cat(sprintf("offset: %s\n", offset))

    # continue to make requests for data if paginate=TRUE and there is data
    while (isTRUE(paginate) && !is.null(offset)) {
        params['offset'] <- offset
        response <- do.call(Dataset.data, c(id=id, filters, params))
        df_page <- response$results
        df <- rbind(df, df_page)
        offset <- response$offset
        cat(sprintf("offset: %s\n", offset))

        # only fetch a maximum of 1,000,000 rows
        if (nrow(df) >= max_records && !is.null(offset)) {
            warning(paste("This call returns more than 1M records, which is larger than Dataset.query() allows.",
                          "Please contact SolveBio Support for help with retrieving more data."), call. = FALSE)
            break
        }
    }

    if (!isTRUE(paginate) && !is.null(offset)) {
        warning(paste("This call returned only the first page of records. To retrieve more pages automatically,"
                      "please set paginate=TRUE when calling Dataset.query().", call. = FALSE))
    }

    return(df)
}
