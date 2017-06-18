#' DatasetField.all
#'
#' Retrieves the metadata about all dataset fields on SolveBio.
#'
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
DatasetField.all <- function(...) {
    .request('GET', "v1/dataset_fields", query=list(...))
}

#' DatasetField.retrieve
#'
#' Retrieves the metadata about a specific dataset field.
#'
#' @param id String The ID or full name of a dataset field.
#'
#' @examples \dontrun{
#' DatasetField.retrieve(691)
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
DatasetField.retrieve <- function(id) {
    if (missing(id)) {
        stop("A dataset field ID or name is required.")
    }

    path <- paste("v1/dataset_fields", paste(id), sep="/")
    .request('GET', path=path)
}

#' DatasetField.facets
#'
#' Returns the facets for a given dataset field.
#'
#' @param id String The ID or full name of a dataset field.
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
DatasetField.facets <- function(id, ...) {
    if (missing(id) | !(class(id) %in% c("DatasetField", "numeric", "character"))) {
        stop("A dataset field ID or name is required.")
    }
    if (class(id) == "DatasetField") {
        id <- id$id
    }

    path <- paste("v1/dataset_fields", paste(id), "facets", sep="/")
    .request('GET', path=path, query=list(...))
}


#' DatasetField.create
#'
#' Create a new dataset field.
#'
#' @param dataset_id The dataset ID.
#' @param name The name of the dataset field.
#' @param data_type (optional) The data type for the field (default: auto).
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
DatasetField.create <- function(dataset_id, name, data_type='auto', ...) {
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

    dataset_field <- .request('POST', path='v1/dataset_fields', query=NULL, body=params)

    return(dataset_field)
}
