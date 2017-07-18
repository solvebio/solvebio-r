#' Object.all
#'
#' Retrieves the metadata about all objects on SolveBio.
#'
#' @param ... (optional) Additional query parameters (e.g. page).
#'
#' @examples \dontrun{
#' Object.all()
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
Object.all <- function(...) {
    .request('GET', "v2/objects", query=list(...))
}

#' Object.retrieve
#'
#' Retrieves the metadata about a specific object from SolveBio.
#'
#' @param id String The ID of a SolveBio object
#'
#' @examples \dontrun{
#' Object.retrieve("ClinVar/ClinVar")
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
Object.retrieve <- function(id) {
    if (missing(id)) {
        stop("A object ID is required.")
    }

    path <- paste("v2/objects", paste(id), sep="/")
    .request('GET', path=path)
}


#' Object.delete
#'
#' Delete a specific object from SolveBio.
#'
#' @param id String The ID of a SolveBio object
#'
#' @examples \dontrun{
#' Object.delete("1")
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
Object.delete <- function(id) {
    if (missing(id)) {
        stop("A object ID or name is required.")
    }

    path <- paste("v2/objects", paste(id), sep="/")
    .request('DELETE', path=path)
}


#' Object.data
#'
#' Returns one page of documents from a SolveBio object and processes the response.
#' @param id The ID of a SolveBio object, or a Object object.
#' @param filters (optional) Query filters.
#' @param ... (optional) Additional query parameters (e.g. limit, offset).
#'
#' @examples \dontrun{
#' Object.data("ClinVar/ClinVar")
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
Object.data <- function(id, filters, ...) {
    if (missing(id) | !(class(id) %in% c("Object", "numeric", "integer", "character"))) {
        stop("A object ID or name is required.")
    }
    if (class(id) == "Object") {
        id <- id$id
    }

    body = list(...)

    # Filters can be passed as a JSON string
    if (!missing(filters) && !is.null(filters) && filters != "") {
        if (class(filters) == "character") {
            # Convert JSON string to an R structure
            filters <- jsonlite::fromJSON(filters)
        }
        # Add filters to request body
        body = modifyList(body, list(filters=filters))
    }

    path <- paste("v2/objects", paste(id), "data", sep="/")

    tryCatch({
        res <- .request('POST', path=path, body=body)
        return(formatSolveBioQueryResponse(res))
    }, error = function(e) {
        cat(sprintf("Query failed: %s\n", e$message))
    })
}


#' Object.query
#'
#' Queries a SolveBio object and returns an R data frame containing all records (up to 500,000).
#' Returns a single page of results otherwise (default).
#' @param id The ID of a SolveBio object, or a Object object.
#' @param paginate When set to TRUE, retrieves up to 500,000 records.
#' @param ... (optional) Additional query parameters (e.g. filters, limit, offset).
#'
#' @examples \dontrun{
#' Object.query("ClinVar/ClinVar", paginate=TRUE)
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
Object.query <- function(id, paginate=FALSE, ...) {
    # Max allowed records (total) when paginating
    max_records = 500000
    params <- list(...)

    # Retrieve the first page of results
    response <- do.call(Object.data, c(id=id, params))
    df <- response$result
    offset <- response$offset

    # continue to make requests for data if pagination is enabled and there are more records
    while (isTRUE(paginate) && !is.null(offset)) {
        params['offset'] <- offset
        response <- do.call(Object.data, c(id=id, params))
        df_page <- response$results
        df <- dplyr::bind_rows(df, df_page)
        offset <- response$offset

        # only fetch max_records
        if (nrow(df) >= max_records && !is.null(offset)) {
            warning(paste("This call returns more than 500,000 records, which is larger than Object.query() allows.",
                          "Please contact SolveBio Support for help with retrieving more data."), call. = FALSE)
            break
        }
    }

    if (!isTRUE(paginate) && !is.null(offset)) {
        warning(paste("This call returned only the first page of records. To retrieve more pages automatically,",
                      "please set paginate=TRUE when calling Object.query().", call. = FALSE))
    }

    return(df)
}


#' Object.facets
#'
#' Retrieves aggregated statistics or term counts for one or more fields in a SolveBio object. Returns a list of data frames, one for each requested facet.
#' @param id The ID of a SolveBio object, or a Object object.
#' @param facets A list of one or more field facets.
#' @param ... (optional) Additional query parameters (e.g. filters, limit, offset).
#'
#' @examples \dontrun{
#' Object.facets("ClinVar/Combined", list("clinical_significance", "gene_symbol"))
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
Object.facets <- function(id, facets, ...) {
    if (missing(facets) || is.null(facets) || facets == "") {
        stop("A list of one or more facets is required.")
    }

    if (class(facets) == "character") {
        if (grepl("[[{]", facets)) {
            # If it looks like JSON, try to convert to an R structure
            facets <- jsonlite::fromJSON(facets)
        }
    }

    params <- list(...)
    # Facet queries should not return results
    params$limit = 0
    params <- modifyList(params, list(facets=facets))

    response <- do.call(Object.data, c(id=id, params))

    return(response$facets)
}


#' Object.count
#'
#' Returns the total number of records for a given SolveBio object.
#' @param id The ID of a SolveBio object, or a Object object.
#' @param ... (optional) Additional query parameters (e.g. filters, limit, offset).
#'
#' @examples \dontrun{
#' Object.count("ClinVar/Variants")
#' Object.count("ClinVar/Variants", filters='[["gene_symbol", "BRCA2"]]')
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
Object.count <- function(id, ...) {
    params <- list(...)
    # Count queries should not return results
    params$limit = 0

    response <- do.call(Object.data, c(id=id, params))

    return(response$total)
}


#' Object.create
#'
#' Create an empty SolveBio object.
#' @param depository_version_id The ID of the parent depository verison.
#' @param name The unique name of the object witin the version.
#' @param ... (optional) Additional object attributes.
#'
#' @examples \dontrun{
#' Object.create(depository_version_id=1, name="MyObject")
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
Object.create <- function(depository_version_id, name, ...) {
    if (missing(depository_version_id)) {
        stop("A depository version ID is required.")
    }
    if (missing(name)) {
        stop("A name is required.")
    }

    # TODO: Use title if available in (...)
    params = list(
                  depository_version_id=depository_version_id,
                  name=name,
                  title=name,
                  ...
                  )

    object <- .request('POST', path='v2/objects', query=NULL, body=params)

    return(object)
}


#' Object.get_or_create_by_full_name
#'
#' A helper function to create a object on SolveBio using a full name.
#' @param full_name A valid object full name (<depository/version/object>).
#' @param ... (optional) Additional object attributes.
#'
#' @examples \dontrun{
#' Object.get_or_create_by_full_name(<FULL NAME>)
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
# Object.get_or_create_by_full_name <- function(full_name, ...) {
#     if (missing(full_name)) {
#         stop("A valid object full name is required.")
#     }
#
#     # Split full name
#     parts <- strsplit(full_name, split='/', fixed=TRUE)[[1]]
#
#     if (length(parts) != 3) {
#         stop("Full names must be in the format: <depository>/<version>/<object>")
#     }
#
#     # Check if Object already exists
#     tryCatch({
#         d <- Object.retrieve(full_name)
#         if (!is.null(d)) {
#             return(d)
#         }
#     }, error = function(e) {})
#
#     depository_name <- parts[[1]]
#     version_name <- parts[[2]]
#     object_name <- parts[[3]]
#
#     # Get or create Depository
#     depository_obj = tryCatch({
#         depository_obj <- Depository.retrieve(depository_name)
#     }, error = function(e) {
#         depository_obj <- Depository.create(depository_name)
#         return(depository_obj)
#     })
#
#     # Get or create DepositoryVersion
#     version_obj = tryCatch({
#         version_full_name <- paste(depository_name, version_name, sep="/")
#         version_obj = DepositoryVersion.retrieve(version_full_name)
#     }, error = function(e) {
#         version_obj <- DepositoryVersion.create(
#                                                 depository_id=depository_obj$id,
#                                                 name=version_name,
#                                                 title=version_name
#                                                 )
#         return(version_obj)
#     })
#
#     # Create the Object
#     object_obj <- Object.create(
#                                   depository_version_id=version_obj$id,
#                                   name=object_name,
#                                   ...
#                                   )
#
#     return(object_obj)
# }
