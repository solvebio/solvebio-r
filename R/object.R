#' Object.all
#'
#' Retrieves the metadata about all objects on SolveBio accessible to the current user.
#'
#' @param ... (optional) Additional query parameters.
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
#' Retrieves the metadata about a specific object on SolveBio.
#'
#' @param id The ID of the object.
#'
#' @examples \dontrun{
#' Object.retrieve("1234567890")
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
#' @param id The ID of the object.
#'
#' @examples \dontrun{
#' Object.delete("1234567890")
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
Object.delete <- function(id) {
    if (missing(id)) {
        stop("A object ID is required.")
    }

    path <- paste("v2/objects", paste(id), sep="/")
    .request('DELETE', path=path)
}


#' Object.create
#'
#' Create a SolveBio object.
#'
#' @param vault_id The target vault ID.
#' @param parent_object_id The ID of the parent object (folder) or NULL for the vault root.
#' @param object_type The type of object (i.e. "folder").
#' @param filename The filename (i.e. the name) of the object.
#' @param ... (optional) Additional object attributes.
#'
#' @examples \dontrun{
#' Object.create(vault_id="1234567890", parent_object_id=NULL, object_type="folder", filename="My Folder")
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
Object.create <- function(vault_id, parent_object_id, object_type, filename, ...) {
    if (missing(vault_id)) {
        stop("A vault ID is required.")
    }
    if (missing(parent_object_id)) {
        parent_object_id = NULL
    }
    if (missing(object_type)) {
        stop("An object type is required: folder, dataset, file")
    }
    if (missing(filename)) {
        stop("A name is required.")
    }

    params = list(
                  vault_id=vault_id,
                  parent_object_id=parent_object_id,
                  object_type=object_type,
                  filename=filename,
                  ...
                  )

    object <- .request('POST', path='v2/objects', query=NULL, body=params)

    return(object)
}


#' Object.get_by_full_path
#'
#' A helper function to get an object on SolveBio by its full path.
#'
#' @param full_path The full path to the object. 
#' @param ... (optional) Additional query parameters.
#'
#' @examples \dontrun{
#' Object.get_by_full_path("SolveBio:Public:/ClinVar")
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
Object.get_by_full_path <- function(full_path, ...) {
    split_path = strsplit(full_path, ":", fixed=TRUE)[[1]]

    if (length(split_path) == 2) {
        # Get the user"s account for them
        user = .request("GET", path="v1/user")
        account_domain = user$account$domain
        name = split_path[[1]]
        full_path = paste(account_domain, name, split_path[[2]], sep=":")
    }

    params = list(
                  full_path=full_path,
                  ...
                  )
    response <- .request('GET', path='v2/objects', query=params)

    if (response$total == 0) {
        return(NULL)
    }
    if (response$total > 1) {
        cat(sprintf("Warning: Multiple object found with full path: %s\n", full_path))
    }

    return(response$data)
}


#' Object.get_by_path
#'
#' A helper function to get an object on SolveBio by its path. Used as a pass-through function from some Vault methods.
#'
#' @param path The path to the object, relative to a vault.
#' @param ... (optional) Additional query parameters.
#'
#' @examples \dontrun{
#' Object.get_by_path("/ClinVar")
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
Object.get_by_path <- function(path, ...) {
    params = list(
                  path=path,
                  ...
                  )
    response <- .request('GET', path='v2/objects', query=params)
    if (response$total > 0) {
        return(response$data[1, ])
    }

    return(NULL)
}


#' Object.get_download_url
#'
#' Helper method to get the download URL for a file object.
#'
#' @param id The ID of the object.
#'
#' @examples \dontrun{
#' Object.get_download_url("1234567890")
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
Object.get_download_url <- function(id) {
    if (missing(id)) {
        stop("A object ID is required.")
    }

    path <- paste("v2/objects", paste(id), "download", sep="/")
    response <- .request('GET', path=path, query=list(redirect=NULL))

    return(response$download_url)
}
