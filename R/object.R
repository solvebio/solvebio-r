#' Object.all
#'
#' Retrieves the metadata about all objects on SolveBio accessible to the current user.
#'
#' @param env (optional) Custom client environment.
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
Object.all <- function(env = solvebio:::.solveEnv, ...) {
    .request('GET', "v2/objects", query=list(...), env=env)
}

#' Object.retrieve
#'
#' Retrieves the metadata about a specific object on SolveBio.
#'
#' @param id The ID of the object.
#' @param env (optional) Custom client environment.
#'
#' @examples \dontrun{
#' Object.retrieve("1234567890")
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
Object.retrieve <- function(id, env = solvebio:::.solveEnv) {
    if (missing(id)) {
        stop("A object ID is required.")
    }

    path <- paste("v2/objects", paste(id), sep="/")
    .request('GET', path=path, env=env)
}


#' Object.delete
#'
#' Delete a specific object from SolveBio.
#'
#' @param id The ID of the object.
#' @param env (optional) Custom client environment.
#'
#' @examples \dontrun{
#' Object.delete("1234567890")
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
Object.delete <- function(id, env = solvebio:::.solveEnv) {
    if (missing(id)) {
        stop("A object ID is required.")
    }

    path <- paste("v2/objects", paste(id), sep="/")
    .request('DELETE', path=path, env=env)
}


#' Object.create
#'
#' Create a SolveBio object.
#'
#' @param vault_id The target vault ID.
#' @param parent_object_id The ID of the parent object (folder) or NULL for the vault root.
#' @param object_type The type of object (i.e. "folder").
#' @param filename The filename (i.e. the name) of the object.
#' @param env (optional) Custom client environment.
#' @param ... (optional) Additional object attributes.
#'
#' @examples \dontrun{
#' Object.create(
#'               vault_id="1234567890",
#'               parent_object_id=NULL,
#'               object_type="folder",
#'               filename="My Folder"
#'               )
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
Object.create <- function(vault_id, parent_object_id, object_type, filename, env = solvebio:::.solveEnv, ...) {
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

    object <- .request('POST', path='v2/objects', query=NULL, body=params, env=env)

    return(object)
}


#' Object.update
#'
#' Updates the attributes of an existing vault object.
#'
#' @param id The ID of the vault to update.
#' @param env (optional) Custom client environment.
#' @param ... Object attributes to change.
#'
#' @examples \dontrun{
#' Object.update(
#'               id="1234",
#'               filename="New Name",
#'              )
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
Object.update <- function(id, env = solvebio:::.solveEnv, ...) {
    if (missing(id)) {
        stop("An object ID is required.")
    }

    params = list(...)

    path <- paste("v2/objects", paste(id), sep="/")
    .request('PATCH', path=path, query=NULL, body=params, env=env)
}


#' Object.get_by_full_path
#'
#' A helper function to get an object on SolveBio by its full path.
#'
#' @param full_path The full path to the object. 
#' @param env (optional) Custom client environment.
#' @param ... (optional) Additional query parameters.
#'
#' @examples \dontrun{
#' Object.get_by_full_path("solvebio:public:/ClinVar")
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
Object.get_by_full_path <- function(full_path, env = solvebio:::.solveEnv, ...) {
    params = list(
                  full_path=full_path,
                  ...
                  )
    response <- .request('GET', path='v2/objects', query=params, env=env)

    if (response$total == 0) {
        stop(sprintf("Error: No object found with full path: %s\n", full_path))
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
#' @param env (optional) Custom client environment.
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
Object.get_by_path <- function(path, env = solvebio:::.solveEnv, ...) {
    # Remove trailing backslash from vault_path
    path = sub("/$", "", path)

    params = list(
                  path=path,
                  ...
                  )
    response <- .request('GET', path='v2/objects', query=params, env=env)
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
#' @param env (optional) Custom client environment.
#'
#' @examples \dontrun{
#' Object.get_download_url("1234567890")
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
Object.get_download_url <- function(id, env = solvebio:::.solveEnv) {
    if (missing(id)) {
        stop("A object ID is required.")
    }

    path <- paste("v2/objects", paste(id), "download", sep="/")
    response <- .request('GET', path=path, query=list(redirect=NULL), env=env)

    return(response$download_url)
}


#' Object.upload_file
#'
#' Upload a local file to a vault on SolveBio. The vault path provided is the parent directory for uploaded file.
#'
#' @param local_path The path to the local file
#' @param vault_id The SolveBio vault ID
#' @param vault_path The remote path in the vault
#' @param filename (optional) The filename for the uploaded file in the vault (default: the basename of the local_path)
#' @param env (optional) Custom client environment.
#'
#' @examples \dontrun{
#' Object.upload_file("my_file.json.gz", vault$id, "/parent/directory/")
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
Object.upload_file <- function(local_path, vault_id, vault_path, filename, env = solvebio:::.solveEnv) {
    if (missing(local_path) || !file.exists(local_path)) {
        stop("A valid path to a local file is required.")
    }
    if (missing(vault_id)) {
        stop("A valid vault ID is required.")
    }
        
    if (missing(filename) || is.null(filename)) {
        filename = basename(local_path)
    }

    if (missing(vault_path) || is.null(vault_path) || vault_path == '/' || vault_path == '') {
        parent_object_id = NULL
    }
    else {
        # Create all folders as necessary in the vault path
        parent_object = Object.get_by_path(path=vault_path, vault_id=vault_id, env=env)
        if (is.null(parent_object)) {
            parent_object = Vault.create_folder(
                                                id=vault_id,
                                                path=vault_path,
                                                recursive=TRUE,
                                                env=env
                                                )
        }
        if (parent_object$object_type != 'folder') {
            stop(sprintf("Error: Vault path is not a valid folder: %s\n", vault_path))
        }
        parent_object_id = parent_object$id
    }

    # Create the file, and upload it to the Upload URL
    obj = Object.create(
                        vault_id=vault_id,
                        parent_object_id=parent_object_id,
                        object_type='file',
                        filename=filename,
                        # md5=digest::digest(file=local_path),
                        size=file.size(local_path),
                        mimetype=mime::guess_type(local_path),
                        env=env
                        )

    # TODO: Get base64_md5 from API
    # base64_md5 = jsonlite::base64_enc(hex2bin(obj$md5))
    headers <- c(
                 # 'Content-MD5'=base64_md5,
                 'Content-Type'=obj$mimetype,
                 'Content-Length'=obj$size
                 )


    res <- httr::PUT(
                     obj$upload_url,
                     httr::add_headers(headers),
                     body=httr::upload_file(local_path, type=obj$mimetype)
                     )
    if (res$status != 200) {
        # Clean up after a failed upload
        Object.delete(obj$id, env=env)
        stop(sprintf("Error: Failed to upload file"))
    }

    return(obj)
}


#' Object.get_or_upload_file
#'
#' Upload a local file to a vault on SolveBio only if it does not yet exist (by name, at the provided path). The vault path provided is the parent directory for uploaded file. Accepts the same arguments as `Object.upload_file`.
#'
#' @param local_path The path to the local file
#' @param vault_id The SolveBio vault ID
#' @param vault_path The remote path in the vault
#' @param filename (optional) The filename for the uploaded file in the vault (default: the basename of the local_path)
#' @param env (optional) Custom client environment.
#'
#' @examples \dontrun{
#' Object.get_or_upload_file("my_file.json.gz", vault$id, "/parent/directory/")
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
Object.get_or_upload_file <- function(local_path, vault_id, vault_path, filename, env = solvebio:::.solveEnv) {
    if (missing(local_path) || !file.exists(local_path)) {
        stop("A valid path to a local file is required.")
    }
    if (missing(vault_id)) {
        stop("A vault ID is required.")
    }
    if (missing(vault_path)) {
        stop("A vault path is required.")
    }
    if (missing(filename) || is.null(filename)) {
        filename <- basename(local_path)
    }

    object_path <- paste(vault_path, filename, sep="/")
    # Remove double slashes
    object_path <- sub("//", "/", object_path)
    object <- Object.get_by_path(path=object_path, vault_id=vault_id, env=env)

    if (is.null(object)) {
        object <- Object.upload_file(local_path, vault_id, vault_path, filename, env=env)
    }

    return(object)
}
