#' Beacon.all
#'
#' Retrieves the metadata about all beacons on SolveBio accessible to the current user.
#'
#' @param ... (optional) Additional query parameters.
#'
#' @examples \dontrun{
#' Beacon.all()
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
Beacon.all <- function(...) {
    .request('GET', "v2/beacons", query=list(...))
}

#' Beacon.retrieve
#'
#' Retrieves the metadata about a specific beacon on SolveBio.
#'
#' @param id The ID of the beacon.
#'
#' @examples \dontrun{
#' Beacon.retrieve("1234")
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
Beacon.retrieve <- function(id) {
    if (missing(id)) {
        stop("A beacon ID is required.")
    }

    path <- paste("v2/beacons", paste(id), sep="/")
    .request('GET', path=path)
}


#' Beacon.delete
#'
#' Delete a specific beacon from SolveBio.
#'
#' @param id The ID of the beacon.
#'
#' @examples \dontrun{
#' Beacon.delete("1234567890")
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
Beacon.delete <- function(id) {
    if (missing(id)) {
        stop("A beacon ID is required.")
    }

    path <- paste("v2/beacons", paste(id), sep="/")
    .request('DELETE', path=path)
}


#' Beacon.create
#'
#' Add a new beacon to an existing beacon set. The beacon set must already exist in order to add beacons.
#'
#' @param beacon_set_id The ID of the parent beacon set.
#' @param vault_object_id The ID of the vault object (i.e. dataset) queried by the beacon.
#' @param title The title displayed for the beacon.
#' @param ... (optional) Additional beacon attributes (such as description and params).
#'
#' @examples \dontrun{
#' Beacon.create(
#'               beacon_set_id="1234",
#'               vault_object_id="1234567890",
#'               title="My new beacon"
#'               )
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
Beacon.create <- function(beacon_set_id, vault_object_id, title, ...) {
    if (missing(beacon_set_id)) {
        stop("A beacon set ID is required.")
    }
    if (missing(vault_object_id)) {
        stop("A vault object ID (dataset ID) is required.")
    }
    if (missing(title)) {
        title = "Untitled Beacon"
    }

    params = list(
                  beacon_set_id=beacon_set_id,
                  vault_object_id=vault_object_id,
                  title=title,
                  ...
                  )

    .request('POST', path='v2/beacons', query=NULL, body=params)
}


#' Beacon.query
#'
#' Query an individual beacon.
#'
#' @param beacon_id The ID of the beacon.
#' @param query The entity ID or query string.
#' @param entity_type (optional) A valid SolveBio entity type.
#' @param ... (optional) Additional query parameters.
#'
#' @examples \dontrun{
#' Beacon.query(
#'               beacon_id="1234",
#'               query="BRCA2",
#'               entity_type="gene"
#'               )
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
Beacon.query <- function(beacon_id, query, entity_type, ...) {
    if (missing(beacon_id)) {
        stop("A beacon ID is required.")
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

    path <- paste("v2/beacons", paste(beacon_id), "query", sep="/")

    .request('POST', path=path, query=NULL, body=params)
}
