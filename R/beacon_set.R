#' BeaconSet.all
#'
#' Retrieves the metadata about all beacon sets on SolveBio accessible to the current user.
#'
#' @param env (optional) Custom client environment.
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
BeaconSet.all <- function(env = solvebio:::.solveEnv, ...) {
    .request('GET', "v2/beacon_sets", query=list(...), env=env)
}

#' BeaconSet.retrieve
#'
#' Retrieves the metadata about a specific beacon set on SolveBio.
#'
#' @param id The ID of the beacon set.
#' @param env (optional) Custom client environment.
#'
#' @examples \dontrun{
#' BeaconSet.retrieve("1234")
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
BeaconSet.retrieve <- function(id, env = solvebio:::.solveEnv) {
    if (missing(id)) {
        stop("A beacon set ID is required.")
    }

    path <- paste("v2/beacon_sets", paste(id), sep="/")
    .request('GET', path=path, env=env)
}


#' BeaconSet.update
#'
#' Updates the attributes of an existing beacon set.
#'
#' @param id The ID of the beacon set to update.
#' @param env (optional) Custom client environment.
#' @param ... Beacon set attributes to change.
#'
#' @examples \dontrun{
#' BeaconSet.update(
#'                  id="1234",
#'                  title="New Beacon Set Title"
#'                 )
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
BeaconSet.update <- function(id, env = solvebio:::.solveEnv, ...) {
    if (missing(id)) {
        stop("A beacon set ID is required.")
    }

    params = list(...)

    path <- paste("v2/beacon_sets", paste(id), sep="/")
    .request('PATCH', path=path, query=NULL, body=params, env=env)
}


#' BeaconSet.delete
#'
#' Delete a specific beacon set (including all its beacons) from SolveBio.
#'
#' @param id The ID of the beacon set.
#' @param env (optional) Custom client environment.
#'
#' @examples \dontrun{
#' BeaconSet.delete("1234")
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
BeaconSet.delete <- function(id, env = solvebio:::.solveEnv) {
    if (missing(id)) {
        stop("A beacon set ID is required.")
    }

    path <- paste("v2/beacon_sets", paste(id), sep="/")
    .request('DELETE', path=path, env=env)
}


#' BeaconSet.create
#'
#' Create a new beacon set.
#'
#' @param title The title displayed for the beacon set.
#' @param description (optional) An optional description for the new beacon set.
#' @param is_shared If TRUE, everyone else in your account will be able to see and query the beacon set, but will not be able to edit it. (Default: FALSE)
#' @param env (optional) Custom client environment.
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
BeaconSet.create <- function(title, description, is_shared=FALSE, env = solvebio:::.solveEnv, ...) {
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

    .request('POST', path='v2/beacon_sets', query=NULL, body=params, env=env)
}


#' BeaconSet.query
#'
#' Query a beacon set (i.e. all the beacons within a beacon set).
#'
#' @param id The ID of the beacon set.
#' @param query The entity ID or query string.
#' @param entity_type (optional) A valid SolveBio entity type.
#' @param env (optional) Custom client environment.
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
BeaconSet.query <- function(id, query, entity_type, env = solvebio:::.solveEnv, ...) {
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

    .request('POST', path=path, query=NULL, body=params, env=env)
}
