#' Dataset.all
#'
#' Retrieves the metadata about datasets on SolveBio.
#'
#' @param env (optional) Custom client environment.
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
Dataset.all <- function(env = solvebio:::.solveEnv, ...) {
    .request('GET', "v2/datasets", query=list(...), env=env)
}


#' Dataset.retrieve
#'
#' Retrieves the metadata about a specific dataset from SolveBio.
#'
#' @param id String The ID of a SolveBio dataset
#' @param env (optional) Custom client environment.
#'
#' @examples \dontrun{
#' Dataset.retrieve("1234567890")
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
Dataset.retrieve <- function(id, env = solvebio:::.solveEnv) {
    if (missing(id)) {
        stop("A dataset ID is required.")
    }

    path <- paste("v2/datasets", paste(id), sep="/")
    .request('GET', path=path, env=env)
}


#' Dataset.delete
#'
#' Delete a specific dataset from SolveBio.
#'
#' @param id String The ID of a SolveBio dataset
#' @param env (optional) Custom client environment.
#'
#' @examples \dontrun{
#' Dataset.delete("1234567890")
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
Dataset.delete <- function(id, env = solvebio:::.solveEnv) {
    if (missing(id)) {
        stop("A dataset ID is required.")
    }

    path <- paste("v2/datasets", paste(id), sep="/")
    .request('DELETE', path=path, env=env)
}


#' Dataset.template
#'
#' Retrieves the template for a dataset.
#'
#' @param id String The ID of a SolveBio dataset
#' @param env (optional) Custom client environment.
#'
#' @examples \dontrun{
#' Dataset.template("1234567890")
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
Dataset.template <- function(id, env = solvebio:::.solveEnv) {
    if (missing(id)) {
        stop("A dataset ID is required.")
    }

    path <- paste("v2/datasets", paste(id), "template", sep="/")
    .request('GET', path=path, env=env)
}


#' Dataset.data
#'
#' Returns one page of documents from a SolveBio dataset and processes the response.
#' @param id The ID of a SolveBio dataset, or a Dataset object.
#' @param filters (optional) Query filters.
#' @param env (optional) Custom client environment.
#' @param ... (optional) Additional query parameters (e.g. limit, offset).
#'
#' @examples \dontrun{
#' Dataset.data("1234567890")
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
Dataset.data <- function(id, filters, env = solvebio:::.solveEnv, ...) {
    if (missing(id) || !(class(id) %in% c("Dataset", "numeric", "integer", "character"))) {
        stop("A dataset ID (or object) is required.")
    }
    if (class(id) == "Dataset" || class(id) == "Object") {
        id <- id$id
    }

    body = list(...)

    # Filters can be passed as a JSON string
    if (!missing(filters) && !is.null(filters) && length(filters) > 0) {
        if (class(filters) == "character") {
            # Convert JSON string to an R structure
            filters <- jsonlite::fromJSON(filters,
                                          simplifyVector = FALSE,
                                          simplifyDataFrame = TRUE,
                                          simplifyMatrix = FALSE)
        }
        # Add filters to request body
        body = modifyList(body, list(filters=filters))
    }

    path <- paste("v2/datasets", paste(id), "data", sep="/")

    tryCatch({
        res <- .request('POST', path=path, body=body, env=env)
        return(formatSolveBioQueryResponse(res))
    }, error = function(e) {
        cat(sprintf("Query failed: %s\n", e$message))
    })
}


#' Dataset.query
#'
#' Queries a SolveBio dataset and returns an R data frame containing all records (up to 500,000).
#' Returns a single page of results otherwise (default).
#'
#' @param id The ID of a SolveBio dataset, or a Dataset object.
#' @param paginate When set to TRUE, retrieves up to 500,000 records.
#' @param env (optional) Custom client environment.
#' @param ... (optional) Additional query parameters (e.g. filters, limit, offset).
#'
#' @examples \dontrun{
#' Dataset.query("12345678790", paginate=TRUE)
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
Dataset.query <- function(id, paginate=FALSE, env = solvebio:::.solveEnv, ...) {
    # Max allowed records (total) when paginating
    max_records = 500000
    params <- list(...)
    params$id <- id
    params$env <- env

    # Retrieve the first page of results
    response <- do.call(Dataset.data, params)
    df <- response$result
    offset <- response$offset

    # continue to make requests for data if pagination is enabled and there are more records
    while (isTRUE(paginate) && !is.null(offset)) {
        params$offset <- offset
        response <- do.call(Dataset.data, params)
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


#' Dataset.fields
#'
#' Retrieves the list of fields and field metadata for a dataset.
#'
#' @param id The ID of a SolveBio dataset, or a Dataset object.
#' @param env (optional) Custom client environment.
#' @param ... (optional) Additional query parameters (e.g. limit, offset).
#'
#' @examples \dontrun{
#' Dataset.fields("1234567890")
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
Dataset.fields <- function(id, env = solvebio:::.solveEnv, ...) {
    if (class(id) == "numeric") {
        warning("Please use string IDs instead of numeric IDs.")
    }

    if (missing(id)) {
        stop("A dataset ID is required.")
    }
    if (class(id) == "Dataset" || class(id) == "Object") {
        id <- id$id
    }

    path <- paste("v2/datasets", paste(id), "fields", sep="/")
    .request('GET', path=path, query=list(...), env=env)
}


#' Dataset.facets
#'
#' Retrieves aggregated statistics or term counts for one or more fields in a SolveBio dataset. Returns a list of data frames, one for each requested facet.
#'
#' @param id The ID of a SolveBio dataset, or a Dataset object.
#' @param facets A list of one or more field facets.
#' @param env (optional) Custom client environment.
#' @param ... (optional) Additional query parameters (e.g. filters, limit, offset).
#'
#' @examples \dontrun{
#' Dataset.facets("1234567890", list("clinical_significance", "gene_symbol"))
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
Dataset.facets <- function(id, facets, env = solvebio:::.solveEnv, ...) {
    if (missing(facets) || is.null(facets) || facets == "") {
        stop("A list of one or more facets is required.")
    }

    if (class(facets) == "character") {
        if (grepl("[[{]", facets)) {
            # If it looks like JSON, try to convert to an R structure
            facets <- jsonlite::fromJSON(facets,
                                         simplifyVector = FALSE,
                                         simplifyDataFrame = TRUE,
                                         simplifyMatrix = FALSE)
        }
    }

    params <- list(...)
    # Facet queries should not return results
    params$limit <- 0
    params$id <- id
    params$env <- env
    params <- modifyList(params, list(facets=facets))

    response <- do.call(Dataset.data, params)

    return(response$facets)
}


#' Dataset.count
#'
#' Returns the total number of records for a given SolveBio dataset.
#' @param id The ID of a SolveBio dataset, or a Dataset object.
#' @param env (optional) Custom client environment.
#' @param ... (optional) Additional query parameters (e.g. filters, limit, offset).
#'
#' @examples \dontrun{
#' dataset <- Dataset.get_by_full_path("solvebio:public:/ClinVar/3.7.4-2017-01-30/Variants-GRCh37")
#' Dataset.count(dataset)
#' Dataset.count(dataset, filters='[["gene_symbol", "BRCA2"]]')
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
Dataset.count <- function(id, env = solvebio:::.solveEnv, ...) {
    params <- list(...)
    # Count queries should not return results
    params$limit <- 0
    params$id <- id
    params$env <- env

    response <- do.call(Dataset.data, params)

    return(response$total)
}


#' Dataset.create
#'
#' Create an empty SolveBio dataset.
#' @param vault_id The ID of the vault.
#' @param vault_parent_object_id The parent object (folder) ID in the vault.
#' @param name The name of the dataset in the parent folder.
#' @param env (optional) Custom client environment.
#' @param ... (optional) Additional dataset attributes.
#'
#' @examples \dontrun{
#' Dataset.create(vault_id=vault$id, vault_parent_object_id=NULL, name="My Dataset")
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
Dataset.create <- function(vault_id, vault_parent_object_id, name, env = solvebio:::.solveEnv, ...) {
    if (missing(vault_id)) {
        stop("A vault ID is required.")
    }
    if (missing(name)) {
        stop("A dataset name is required")
    }
    if (missing(vault_parent_object_id)) {
        # Use the root of the vault (/)
        vault_parent_object_id = NULL
    }

    params = list(
                  vault_id=vault_id,
                  vault_parent_object_id=vault_parent_object_id,
                  name=name,
                  ...
                  )

    dataset <- .request('POST', path='v2/datasets', query=NULL, body=params, env=env)

    return(dataset)
}


#' Dataset.update
#'
#' Updates the attributes of an existing dataset.
#'
#' @param id The ID of the dataset to update.
#' @param env (optional) Custom client environment.
#' @param ... Dataset attributes to change.
#'
#' @examples \dontrun{
#' Dataset.update(
#'                id="1234",
#'                name="New Dataset Name",
#'               )
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
Dataset.update <- function(id, env = solvebio:::.solveEnv, ...) {
    if (missing(id)) {
        stop("A dataset ID is required.")
    }

    params = list(...)

    path <- paste("v2/datasets", paste(id), sep="/")
    .request('PATCH', path=path, query=NULL, body=params, env=env)
}


#' Dataset.get_by_full_path
#'
#' A helper function to get a dataset by its full path.
#'
#' @param full_path A valid full path to a dataset.
#' @param env (optional) Custom client environment.
#'
#' @examples \dontrun{
#' Dataset.get_by_full_path("solvebio:public:/ClinVar/3.7.4-2017-01-30/Variants-GRCh37")
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
Dataset.get_by_full_path <- function(full_path, env = solvebio:::.solveEnv) {
    object = Object.get_by_full_path(full_path, env=env)

    dataset = do.call(Dataset.retrieve, list(id=object$dataset_id, env=env))
    return(dataset)
}


#' Dataset.get_or_create_by_full_path
#'
#' A helper function to get or create a dataset by its full path.
#'
#' @param full_path A valid full path to a dataset.
#' @param env (optional) Custom client environment.
#' @param ... Additional dataset creation parameters.
#'
#' @examples \dontrun{
#' Dataset.get_or_create_by_full_path("MyVault:/folder/sub-folder/dataset")
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
Dataset.get_or_create_by_full_path <- function(full_path, env = solvebio:::.solveEnv, ...) {
    dataset = NULL
    tryCatch({
        dataset <- Dataset.get_by_full_path(full_path=full_path, env=env)
        if (!is.null(dataset)) {
            return(dataset)
        }
    }, error = function(e) {})

    vault <- Vault.get_by_full_path(full_path, env=env)
    if (is.null(vault)) {
        stop(sprintf("Invalid vault in full path: %s\n", full_path))
    }

    # Replace double slashes
    full_path <- sub('//+', '/', full_path)

    # Extract the object path (everything after the first forward slash)
    object_path <- sub(".*?/", "/", full_path)
    if (object_path == full_path) {
        stop(sprintf("Invalid full path: must contain at least one forward slash"))
    }

    # Remove the filename from the path to get the parent directory
    parts <- strsplit(object_path, split='/', fixed=TRUE)[[1]]
    dataset_name = parts[[length(parts)]]
    if (is.null(dataset_name) || dataset_name == "") {
        stop(sprintf("Dataset name cannot be blank"))
    }
    dirs = utils::head(parts, -1)
    parent_path = paste(dirs, collapse="/")

    if (parent_path == "") {
        parent_object_id = NULL
    }
    else {
        # Get or create the parent folder
        parent_object = Object.get_by_path(path=parent_path, vault_id=vault$id, env=env)
        if (is.null(parent_object) || (is.data.frame(parent_object) && nrow(parent_object) == 0)) {
            parent_object = Vault.create_folder(
                                                id=vault$id,
                                                path=parent_path,
                                                recursive=TRUE,
                                                env=env
                                                )
        }
        parent_object_id = parent_object$id
    }

    # Create the dataset under parent_object_id
    dataset = Dataset.create(
                             vault_id=vault$id,
                             vault_parent_object_id=parent_object_id,
                             name=dataset_name,
                             env=env,
                             ...
                             )

    return(dataset)
}


#' Dataset.activity
#'
#' A helper function to get or follow the current activity on a dataset.
#'
#' @param id String The ID of a SolveBio dataset
#' @param follow Follow active tasks until they complete.
#' @param env (optional) Custom client environment.
#'
#' @examples \dontrun{
#' Dataset.activity("1234567890")
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
Dataset.activity <- function(id, follow=TRUE, env = solvebio:::.solveEnv) {
    status <- paste('running', 'queued', 'pending', sep=',')
    tasks <- Task.all(target_object_id=id, status=status, env=env)$data

    if (!follow) {
        return(tasks)
    }

    while (!is.null(nrow(tasks)) && nrow(tasks) > 0) {
        cat(paste("Following", nrow(tasks), "task(s)...\n", sep=" "))
        for(i in 1:length(tasks$id)){
            Task.follow(tasks$id[i])
        }

        Sys.sleep(4)
        tasks <- Task.all(target_object_id=id, status=status, env=env)$data
    }

    cat("No active tasks found.\n")
}
