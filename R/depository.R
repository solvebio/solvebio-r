#' Depository.all
#'
#' Retrieves the metadata about all depositories on SolveBio.
#'
#' @param ... (optional) Additional query parameters (e.g. limit, offset).
#'
#' @examples \dontrun{
#' Depository.all()
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
Depository.all <- function(...) {
    .request('GET', "v1/depositories", query=list(...))
}

#' Depository.retrieve
#'
#' Retrieves the metadata about a specific SolveBio depository.
#'
#' @param id String The ID or full name of a SolveBio depository
#'
#' @examples \dontrun{
#' Depository.retrieve("ClinVar")
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
Depository.retrieve <- function(id) {
    if (missing(id)) {
        stop("A depository ID or name is required.")
    }

    path <- paste("v1/depositories", paste(id), sep="/")
    .request('GET', path=path)
}

#' Depository.versions
#'
#' Returns the list of versions for a given depository.
#'
#' @param id String The ID or full name of a SolveBio depository.
#' @param ... (optional) Additional query parameters (e.g. limit, offset).
#'
#' @examples \dontrun{
#' Depository.versions("ClinVar")
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
Depository.versions <- function(id, ...) {
    if (missing(id) | !(class(id) %in% c("Depository", "numeric", "character"))) {
        stop("A depository ID or name is required.")
    }
    if (class(id) == "Depository") {
        id <- id$id
    }

    path <- paste("v1/depositories", paste(id), "versions", sep="/")
    .request('GET', path=path, query=list(...))
}


#' Depository.latest_version
#'
#' Retrieves the latest version for a given depository.
#'
#' @param id String The ID or full name of a depository, or a Depository object.
#' @param ... (optional) Additional query parameters.
#'
#' @examples \dontrun{
#' Depository.latest_version("ClinVar")
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
Depository.latest_version <- function(id, ...) {
    if (missing(id)) {
        stop("A depository ID or name is required.")
    }
    if (class(id) != "Depository") {
        # First we need to retrieve the Depository
        id <- Depository.retrieve(id)
    }
        
    # id is Depository
    path <- paste("v1/depository_versions", paste(id$latest_version), sep="/")
    .request('GET', path=path, query=list(...))
}


#' Depository.create
#'
#' Create a new SolveBio depository.
#' @param name The unique name of the depository.
#' @param ... (optional) Additional depository attributes.
#'
#' @examples \dontrun{
#' Depository.create(name="my-domain:MyDepository")
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
Depository.create <- function(name, ...) {
    if (missing(name)) {
        stop("A name is required.")
    }

    params = list(
                  name=name,
                  ...
                  )

    depository <- .request('POST', path='v1/depositories', query=NULL, body=params)

    return(depository)
}
