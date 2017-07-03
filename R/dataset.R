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


#' Dataset.delete
#'
#' Delete a specific dataset from SolveBio.
#'
#' @param id String The ID or full name of a SolveBio dataset
#'
#' @examples \dontrun{
#' Dataset.delete("1")
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
Dataset.delete <- function(id) {
    if (missing(id)) {
        stop("A dataset ID or name is required.")
    }

    path <- paste("v1/datasets", paste(id), sep="/")
    .request('DELETE', path=path)
}


#' Dataset.data
#'
#' Returns one page of documents from a SolveBio dataset and processes the response.
#' @param id The ID or full name of a SolveBio dataset, or a Dataset object.
#' @param filters (optional) Query filters.
#' @param ... (optional) Additional query parameters (e.g. limit, offset).
#'
#' @examples \dontrun{
#' Dataset.data("ClinVar/ClinVar")
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
Dataset.data <- function(id, filters, ...) {
    if (missing(id) | !(class(id) %in% c("Dataset", "numeric", "integer", "character"))) {
        stop("A dataset ID or name is required.")
    }
    if (class(id) == "Dataset") {
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

    path <- paste("v1/datasets", paste(id), "data", sep="/")

    tryCatch({
        res <- .request('POST', path=path, body=body)
        return(formatSolveBioQueryResponse(res))
    }, error = function(e) {
        cat(sprintf("Query failed: %s\n", e$message))
    })
}


#' Dataset.query
#'
#' Queries a SolveBio dataset and returns an R data frame containing all records (up to 500,000).
#' Returns a single page of results otherwise (default).
#' @param id The ID or full name of a SolveBio dataset, or a Dataset object.
#' @param paginate When set to TRUE, retrieves up to 500,000 records.
#' @param ... (optional) Additional query parameters (e.g. filters, limit, offset).
#'
#' @examples \dontrun{
#' Dataset.query("ClinVar/ClinVar", paginate=TRUE)
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
Dataset.query <- function(id, paginate=FALSE, ...) {
    # Max allowed records (total) when paginating
    max_records = 500000
    params <- list(...)

    # Retrieve the first page of results
    response <- do.call(Dataset.data, c(id=id, params))
    df <- response$result
    offset <- response$offset

    # continue to make requests for data if pagination is enabled and there are more records
    while (isTRUE(paginate) && !is.null(offset)) {
        params['offset'] <- offset
        response <- do.call(Dataset.data, c(id=id, params))
        df_page <- response$results
        df <- dplyr::bind_rows(df, df_page)
        offset <- response$offset

        # only fetch max_records
        if (nrow(df) >= max_records && !is.null(offset)) {
            warning(paste("This call returns more than 500,000 records, which is larger than Dataset.query() allows.",
                          "Please contact SolveBio Support for help with retrieving more data."), call. = FALSE)
            break
        }
    }

    if (!isTRUE(paginate) && !is.null(offset)) {
        warning(paste("This call returned only the first page of records. To retrieve more pages automatically,",
                      "please set paginate=TRUE when calling Dataset.query().", call. = FALSE))
    }

    return(df)
}


#' Dataset.facets
#'
#' Retrieves aggregated statistics or term counts for one or more fields in a SolveBio dataset. Returns a list of data frames, one for each requested facet.
#' @param id The ID or full name of a SolveBio dataset, or a Dataset object.
#' @param facets A list of one or more field facets.
#' @param ... (optional) Additional query parameters (e.g. filters, limit, offset).
#'
#' @examples \dontrun{
#' Dataset.facets("ClinVar/Combined", list("clinical_significance", "gene_symbol"))
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
Dataset.facets <- function(id, facets, ...) {
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

    response <- do.call(Dataset.data, c(id=id, params))

    return(response$facets)
}


#' Dataset.count
#'
#' Returns the total number of records for a given SolveBio dataset.
#' @param id The ID or full name of a SolveBio dataset, or a Dataset object.
#' @param ... (optional) Additional query parameters (e.g. filters, limit, offset).
#'
#' @examples \dontrun{
#' Dataset.count("ClinVar/Variants")
#' Dataset.count("ClinVar/Variants", filters='[["gene_symbol", "BRCA2"]]')
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
Dataset.count <- function(id, ...) {
    params <- list(...)
    # Count queries should not return results
    params$limit = 0

    response <- do.call(Dataset.data, c(id=id, params))

    return(response$total)
}


#' Dataset.create
#'
#' Create an empty SolveBio dataset.
#' @param depository_version_id The ID of the parent depository verison.
#' @param name The unique name of the dataset witin the version.
#' @param ... (optional) Additional dataset attributes.
#'
#' @examples \dontrun{
#' Dataset.create(depository_version_id=1, name="MyDataset")
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
Dataset.create <- function(depository_version_id, name, ...) {
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

    dataset <- .request('POST', path='v1/datasets', query=NULL, body=params)

    return(dataset)
}


#' Dataset.get_or_create_by_full_name
#'
#' A helper function to create a dataset on SolveBio using a full name.
#' @param full_name A valid dataset full name (<depository/version/dataset>).
#' @param ... (optional) Additional dataset attributes.
#'
#' @examples \dontrun{
#' Dataset.get_or_create_by_full_name(<FULL NAME>)
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
Dataset.get_or_create_by_full_name <- function(full_name, ...) {
    if (missing(full_name)) {
        stop("A valid dataset full name is required.")
    }

    # Split full name
    parts <- strsplit(full_name, split='/', fixed=TRUE)[[1]]

    if (length(parts) != 3) {
        stop("Full names must be in the format: <depository>/<version>/<dataset>")
    }

    # Check if Dataset already exists
    tryCatch({
        d <- Dataset.retrieve(full_name)
        if (!is.null(d)) {
            return(d)
        }
    }, error = function(e) {})

    depository_name <- parts[[1]]
    version_name <- parts[[2]]
    dataset_name <- parts[[3]]

    # Get or create Depository
    depository_obj = tryCatch({
        depository_obj <- Depository.retrieve(depository_name)
    }, error = function(e) {
        depository_obj <- Depository.create(depository_name)
        return(depository_obj)
    })

    # Get or create DepositoryVersion
    version_obj = tryCatch({
        version_full_name <- paste(depository_name, version_name, sep="/")
        version_obj = DepositoryVersion.retrieve(version_full_name)
    }, error = function(e) {
        version_obj <- DepositoryVersion.create(
                                                depository_id=depository_obj$id,
                                                name=version_name,
                                                title=version_name
                                                )
        return(version_obj)
    })

    # Create the Dataset
    dataset_obj <- Dataset.create(
                                  depository_version_id=version_obj$id,
                                  name=dataset_name,
                                  ...
                                  )

    return(dataset_obj)
}
