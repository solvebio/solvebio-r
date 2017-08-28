#' BeaconSet.all
#'
#' Retrieves the metadata about all beacon sets on SolveBio accessible to the current user.
#'
#' @param ... (optional) Additional query parameters.
#'
#' @examples \dontrun{
#' BeaconSet.all()
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
BeaconSet.all <- function(...) {
    .request('GET', "v2/beacon_sets", query=list(...))
}

#' BeaconSet.retrieve
#'
#' Retrieves the metadata about a specific beacon set on SolveBio.
#'
#' @param id The ID of the beacon set.
#'
#' @examples \dontrun{
#' BeaconSet.retrieve("1234")
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
BeaconSet.retrieve <- function(id) {
    if (missing(id)) {
        stop("A beacon set ID is required.")
    }

    path <- paste("v2/beacon_sets", paste(id), sep="/")
    .request('GET', path=path)
}


#' BeaconSet.delete
#'
#' Delete a specific beacon set (including all its beacons) from SolveBio.
#'
#' @param id The ID of the beacon set.
#'
#' @examples \dontrun{
#' BeaconSet.delete("1234")
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
BeaconSet.delete <- function(id) {
    if (missing(id)) {
        stop("A beacon set ID is required.")
    }

    path <- paste("v2/beacon_sets", paste(id), sep="/")
    .request('DELETE', path=path)
}


#' BeaconSet.create
#'
#' Create a new beacon set.
#'
#' @param title The title displayed for the beacon set.
#' @param description (optional) An optional description for the new beacon set.
#' @param is_shared If TRUE, everyone else in your account will be able to see and query the beacon set, but will not be able to edit it. (Default: FALSE)
#' @param ... (optional) Additional beacon set attributes.
#'
#' @examples \dontrun{
#' BeaconSet.create(
#'                  title="My new beacon set",
#'                  )
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
BeaconSet.create <- function(title, description, is_shared=FALSE, ...) {
    if (missing(title)) {
        stop("A title is required.")
    }
    if (missing(description)) {
        description = ""
    }

    params = list(
                  title=title,
                  description=description,
                  is_shared=is_shared,
                  ...
                  )

    .request('POST', path='v2/beacon_sets', query=NULL, body=params)
}


#' BeaconSet.query
#'
#' Query a beacon set (i.e. all the beacons within a beacon set).
#'
#' @param id The ID of the beacon set.
#' @param query The entity ID or query string.
#' @param entity_type (optional) A valid SolveBio entity type.
#' @param ... (optional) Additional query parameters.
#'
#' @examples \dontrun{
#' BeaconSet.query(
#'                 id="1234",
#'                 query="BRCA2",
#'                 entity_type="gene"
#'                 )
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
BeaconSet.query <- function(id, query, entity_type, ...) {
    if (missing(id)) {
        stop("A beacon set ID is required.")
    }
    if (missing(query)) {
        stop("A query is required.")
    }
    if (missing(entity_type)) {
        entity_type = NULL
    }

    params = list(
                  query=query,
                  entity_type=entity_type,
                  ...
                  )

    path <- paste("v2/beacon_sets", paste(id), "query", sep="/")

    .request('POST', path=path, query=NULL, body=params)
}
