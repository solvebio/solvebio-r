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
Dataset.all <- function(env = .solveEnv, ...) {
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
Dataset.retrieve <- function(id, env = .solveEnv) {
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
Dataset.delete <- function(id, env = .solveEnv) {
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
Dataset.template <- function(id, env = .solveEnv) {
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
Dataset.data <- function(id, filters,  env = .solveEnv, ...) {
    if (missing(id) || !(class(id) %in% c("Dataset", "numeric", "integer", "character"))) {
        stop("A dataset ID (or object) is required.")
    }
    if (inherits(id, "Dataset") || inherits(id, "Object")) {
        id <- id$id
    }
    body = list(...)
    # Filters can be passed as a JSON string
    if (!missing(filters) && !is.null(filters) && length(filters) > 0) {
        if (inherits(filters, "character")) {
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
        res <- formatSolveBioQueryResponse(res)

        return(res)
    }, error = function(e) {
        cat(sprintf("Query failed: %s\n", e$message))
    })
}


#' Dataset.query
#'
#' Queries a SolveBio dataset and returns an R data frame containing all records.
#' Returns a single page of results otherwise (default).
#'
#' @param id The ID of a SolveBio dataset, or a Dataset object.
#' @param paginate When set to TRUE, retrieves all records (memory permitting).
#' @param env (optional) Custom client environment.
#' @param use_field_titles (optional) Use field title instead of field name for query.
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
Dataset.query <- function(id, paginate=FALSE, use_field_titles=TRUE, env = .solveEnv, ...) {
    params <- list(...)
    params$id <- id
    params$env <- env

    # Retrieve the first page of results
    response <- do.call(Dataset.data, params)
    df <- response$result
    offset <- response$offset

    if (response$total > 100000 && isTRUE(paginate)) {
        warning(paste("This query will retrieve ", response$total, " records, which may take some time..."), call. = FALSE)
    }

    # Continue to make requests for data if pagination is enabled and there are more records
    while (isTRUE(paginate) && !is.null(offset)) {
        params$offset <- offset
        response <- do.call(Dataset.data, params)
        df <- jsonlite::rbind_pages(list(df, response$results))
        offset <- response$offset
    }

    if (!isTRUE(paginate) && !is.null(offset)) {
        warning(paste("This call returned only the first page of records. To retrieve more pages automatically,",
                      "please set paginate=TRUE when calling Dataset.query().", call. = FALSE))
    }

    df <- formatQueryColumns(id, env, df, use_field_titles)

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
Dataset.fields <- function(id, env = .solveEnv, ...) {
    if (inherits(id, "numeric")) {
        warning("Please use string IDs instead of numeric IDs.")
    }

    if (missing(id)) {
        stop("A dataset ID is required.")
    }

    if (inherits(id, "Dataset") || inherits(id, "Object")) {
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
Dataset.facets <- function(id, facets, env = .solveEnv, ...) {
    if (missing(facets) || is.null(facets) || facets == "") {
        stop("A list of one or more facets is required.")
    }

    if (inherits(facets, "character")) {
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
Dataset.count <- function(id, env = .solveEnv, ...) {
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
Dataset.create <- function(vault_id, vault_parent_object_id, name, env = .solveEnv, ...) {
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
Dataset.update <- function(id, env = .solveEnv, ...) {
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
Dataset.get_by_full_path <- function(full_path, env = .solveEnv) {
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
Dataset.get_or_create_by_full_path <- function(full_path, env = .solveEnv, ...) {
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
Dataset.activity <- function(id, follow=TRUE, env = .solveEnv) {
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

#' Dataset.get_global_beacon_status
#'
#' Retrieves the global beacon status for the dataset.
#'
#' @param id The ID of a SolveBio dataset.
#' @param raise_on_disabled Whether to raise an exception if Global Beacon is disabled or to return NULL.
#' @param env (optional) Custom client environment.
#'
#' @examples \dontrun{
#' Dataset.get_global_beacon_status("1234567890")
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
Dataset.get_global_beacon_status <- function(id, raise_on_disabled = FALSE, env = .solveEnv) {
    if (missing(id)) {
        stop("A dataset ID is required.")
    }

    return(Object.get_global_beacon_status(id, raise_on_disabled=raise_on_disabled, env=env))
}


#' Dataset.enable_global_beacon
#'
#' Enables Global Beacon for the the dataset.
#'
#' @param id The ID of a SolveBio dataset.
#' @param env (optional) Custom client environment.
#'
#' @examples \dontrun{
#' Dataset.enable_global_beacon("1234567890")
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
Dataset.enable_global_beacon <- function(id, env = .solveEnv) {
    if (missing(id)) {
        stop("A dataset ID is required.")
    }

    return(Object.enable_global_beacon(id, env=env))
}


#' Dataset.disable_global_beacon
#'
#' Disables Global Beacon for the dataset.
#'
#' @param id The ID of a SolveBio dataset.
#' @param env (optional) Custom client environment.
#'
#' @examples \dontrun{
#' Dataset.disable_global_beacon("1234567890")
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
Dataset.disable_global_beacon <- function(id, env = .solveEnv) {
    if (missing(id)) {
        stop("A dataset ID is required.")
    }

    return(Object.disable_global_beacon(id, env=env))
}



