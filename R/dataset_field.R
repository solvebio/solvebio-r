#' DatasetField.all
#'
#' Retrieves the metadata about all dataset fields on SolveBio.
#'
#' @param env (optional) Custom client environment.
#' @param ... (optional) Additional query parameters (e.g. page).
#'
#' @examples \dontrun{
#' DatasetField.all()
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
DatasetField.all <- function(env = .solveEnv, ...) {
    .request('GET', "v2/dataset_fields", query=list(...), env=env)
}


#' DatasetField.retrieve
#'
#' Retrieves the metadata about a specific dataset field.
#'
#' @param id String The ID of a dataset field.
#' @param env (optional) Custom client environment.
#'
#' @examples \dontrun{
#' DatasetField.retrieve(691)
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
DatasetField.retrieve <- function(id, env = .solveEnv) {
    if (missing(id)) {
        stop("A dataset field ID is required.")
    }

    path <- paste("v2/dataset_fields", paste(id), sep="/")
    .request('GET', path=path, env=env)
}


#' DatasetField.facets
#'
#' Returns the facets for a given dataset field.
#'
#' @param id String The ID of a dataset field.
#' @param env (optional) Custom client environment.
#' @param ... (optional) Additional query parameters.
#'
#' @examples \dontrun{
#' DatasetField.facets(691)
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
DatasetField.facets <- function(id, env = .solveEnv, ...) {
    if (missing(id) | !(class(id) %in% c("DatasetField", "numeric", "character"))) {
        stop("A dataset field ID is required.")
    }
    if (inherits(id, "DatasetField")) {
        id <- id$id
    }

    path <- paste("v2/dataset_fields", paste(id), "facets", sep="/")
    .request('GET', path=path, query=list(...), env=env)
}


#' DatasetField.create
#'
#' Create a new dataset field.
#'
#' @param dataset_id The dataset ID.
#' @param name The name of the dataset field.
#' @param data_type (optional) The data type for the field (default: auto).
#' @param env (optional) Custom client environment.
#' @param ... (optional) Additional dataset import attributes.
#'
#' @examples \dontrun{
#' DatasetField.create(dataset_id=<ID>, name="my_field", title="My Field", data_type="string")
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
DatasetField.create <- function(dataset_id, name, data_type = 'auto', env = .solveEnv, ...) {
    if (missing(dataset_id)) {
        stop("A dataset ID is required.")
    }
    if (missing(name)) {
        stop("A field name is required.")
    }

    params = list(
                  dataset_id=dataset_id,
                  name=name,
                  data_type=data_type,
                  ...
                  )

    dataset_field <- .request('POST', path='v2/dataset_fields', query=NULL, body=params, env=env)

    return(dataset_field)
}


#' DatasetField.update
#'
#' Updates the attributes of an existing dataset field. NOTE: The data_type of a field cannot be changed.
#'
#' @param id The ID of the dataset field to update.
#' @param env (optional) Custom client environment.
#' @param ... Dataset field attributes to change.
#'
#' @examples \dontrun{
#' DatasetField.update(
#'                     id="1234",
#'                     title="New Field Title"
#'                    )
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
DatasetField.update <- function(id, env = .solveEnv, ...) {
    if (missing(id)) {
        stop("A dataset field ID is required.")
    }

    params = list(...)

    path <- paste("v2/dataset_fields", paste(id), sep="/")
    .request('PATCH', path=path, query=NULL, body=params, env=env)
}
