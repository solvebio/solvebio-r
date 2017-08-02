#' Vault.all
#'
#' Retrieves the metadata about all accessible vaults.
#'
#' @param ... (optional) Additional query parameters (e.g. limit, offset).
#'
#' @examples \dontrun{
#' Vault.all()
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
Vault.all <- function(...) {
    .request("GET", "v2/vaults", query=list(...))
}


#' Vault.retrieve
#'
#' Retrieves the metadata about a specific SolveBio vault.
#'
#' @param id String The ID of a SolveBio vault
#'
#' @examples \dontrun{
#' Vault.retrieve("1234567890")
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
Vault.retrieve <- function(id) {
    if (missing(id)) {
        stop("A vault ID is required.")
    }

    path <- paste("v2/vaults", paste(id), sep="/")
    .request("GET", path=path)
}


#' Vault.delete
#'
#' Delete a specific vault from SolveBio. This operation cannot be undone.
#'
#' @param id String The ID of a SolveBio vault.
#'
#' @examples \dontrun{
#' Vault.delete("1")
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
Vault.delete <- function(id) {
    if (missing(id)) {
        stop("A vault ID is required.")
    }

    path <- paste("v2/vaults", paste(id), sep="/")
    .request("DELETE", path=path)
}


#' Vault.create
#'
#' Create a new SolveBio vault.
#' @param name The unique name of the vault.
#' @param ... (optional) Additional vault attributes.
#'
#' @examples \dontrun{
#' Vault.create(name="my-domain:MyVault")
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
Vault.create <- function(name, ...) {
    # TODO
    if (missing(name)) {
        stop("A name is required.")
    }

    params = list(
                  name=name,
                  ...
                  )

    vault <- .request("POST", path="v2/vaults", query=NULL, body=params)

    return(vault)
}


#
# Vault Helpers
#


#' Vault.get_by_full_path
#'
#' Retrieves a specific vault by its full path (domain:vault).
#'
#' @param full_path The full path of a SolveBio vault.
#' @param verbose Print warning/error messages (default: TRUE).
#'
#' @examples \dontrun{
#' Vault.get_by_full_path("SolveBio:Public")
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
Vault.get_by_full_path <- function(full_path, verbose=TRUE) {
    if (missing(full_path)) {
        stop("A vault full path is required.")
    }

    split_path = strsplit(full_path, ":", fixed=TRUE)[[1]]

    if (length(split_path) == 1) {
        # Get the user"s account for them
        user = .request("GET", path="v1/user")
        account_domain = user$account$domain
        name = split_path[[1]]
    }
    if (length(split_path) == 2) {
        # Full path is provided
        account_domain = split_path[[1]]
        name = split_path[[2]]
    }

    params = list(
                  account_domain=account_domain,
                  name=name
                  )
    response = .request("GET", path="v2/vaults", query=params)

    if (response$total == 1) {
        # Found exactly 1 vault
        return(response$data[1, ])
    }

    if (verbose) {
        if (response$total == 0) {
            cat(sprintf("Warning: Could not find vault with full path: %s\n", full_path))
        }

        if (response$total > 1) {
            cat(sprintf("Error: Multiple vaults found with full path: %s\n", full_path))
        }
    }

    return(NULL)
}


#' Vault.get_or_create_by_full_path
#'
#' Retrieves or creates a specific vault by its full path (domain:vault).
#'
#' @param full_path The full path of a SolveBio vault.
#' @param ... (optional) Additional parameters.
#'
#' @examples \dontrun{
#' Vault.get_or_create_by_full_path("My New Vault")
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
Vault.get_or_create_by_full_path <- function(full_path, ...) {
    vault = Vault.get_by_full_path(full_path, verbose=FALSE)
    if (!is.null(vault)) {
        # Return if exists
        return(vault)
    }

    split_path = strsplit(full_path, ":")[[1]]
    name = split_path[length(split_path)]
    vault = Vault.create(name=name, ...)

    return(vault)
}


#' Vault.get_personal_vault
#'
#' Retrieves the current users"s personal, private vault.
#'
#' @examples \dontrun{
#' Vault.get_personal_vault()
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
Vault.get_personal_vault <- function() {
    user = .request("GET", path="v1/user")
    params = list(
                  name=paste("user", user$id, sep="-"),
                  vault_type="user"
                  )

    response = .request("GET", path="v2/vaults", query=params)

    return(response$data[1, ])
}


#' Vault.files
#'
#' Retrieves all files in a specific vault.
#'
#' @param ... (optional) Additional query parameters (e.g. limit, offset).
#'
#' @param id The ID of the vault.
#' @examples \dontrun{
#' vault = Vault.get_personal_vault()
#' Vault.files(vault$id)
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
Vault.files <- function(id, ...) {
    objects = .object_list_helper(id, object_type="file", ...)
    return(objects)
}


#' Vault.folders
#'
#' Retrieves all folders in a specific vault.
#'
#' @param id The ID of the vault.
#' @param ... (optional) Additional query parameters (e.g. limit, offset).
#'
#' @examples \dontrun{
#' vault = Vault.get_personal_vault()
#' Vault.folders(vault$id)
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
Vault.folders <- function(id, ...) {
    objects = .object_list_helper(id, object_type="folder", ...)
    return(objects)
}


#' Vault.datasets
#'
#' Retrieves all datasets in a specific vault.
#'
#' @param id The ID of the vault.
#' @param ... (optional) Additional query parameters (e.g. limit, offset).
#'
#' @examples \dontrun{
#' vault = Vault.get_personal_vault()
#' Vault.datasets(vault$id)
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
Vault.datasets <- function(id, ...) {
    objects = .object_list_helper(id, object_type="dataset", ...)
    return(objects)
}


#' Vault.objects
#'
#' Retrieves all objects in a specific vault.
#'
#' @param id The ID of the vault.
#' @param ... (optional) Additional query parameters (e.g. limit, offset).
#'
#' @examples \dontrun{
#' vault = Vault.get_personal_vault()
#' Vault.objects(vault$id)
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
Vault.objects <- function(id, ...) {
    objects = .object_list_helper(id, ...)
    return(objects)
}


#' Vault.search
#'
#' Search for objects in a specific vault.
#'
#' @param id The ID of the vault.
#' @param query The search query.
#' @param ... (optional) Additional query parameters (e.g. limit, offset).
#'
#' @examples \dontrun{
#' vault = Vault.get_personal_vault()
#' Vault.search('test')
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
Vault.search <- function(id, query, ...) {
    objects = .object_list_helper(id, query=query, ...)
    return(objects)
}


#' Vault.create_dataset
#'
#' Create a new dataset in a vault.
#'
#' @param id The ID of the vault.
#' @param path The path to the dataset, within the vault.
#' @param name The name (filename) for the dataset.
#' @param ... (optional) Additional dataset creation parameters.
#'
#' @examples \dontrun{
#' vault = Vault.get_personal_vault()
#' Vault.create_dataset(vault$id, path="/", name="My Dataset")
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
Vault.create_dataset <- function(id, path, name, ...) {
    if (missing(id)) {
        stop("A vault ID is required.")
    }

    vault = Vault.retrieve(id)

    if (missing(path) || path == "/" || is.null(path)) {
        vault_parent_object_id = NULL
    }
    else {
        user = .request("GET", path="v1/user")
        account_domain = user$account$domain
        full_path = paste(account_domain, vault$name, path, sep=":")
        # Find the parent object (folder) at the provided path
        parent_object = Object.get_by_full_path(full_path)
        vault_parent_object_id = parent_object$id
    }

    dataset = Dataset.create(
                   vault_id=vault$id,
                   vault_parent_object_id=vault_parent_object_id,
                   name=name,
                   ...)

    return(dataset)
}


#' Vault.create_folder
#'
#' Create a new folder in a vault.
#'
#' @param id The ID of the vault.
#' @param path The path to the folder, within the vault.
#' @param recursive Create all parent directories that do not yet exist (default: FALSE).
#' @param ... (optional) Additional folder creation parameters.
#'
#' @examples \dontrun{
#' vault = Vault.get_personal_vault()
#' Vault.create_folder(vault$id, "/My Folder")
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
Vault.create_folder <- function(id, path, recursive=FALSE, ...) {
    if (missing(id) || is.null(id)) {
        stop("A vault ID is required.")
    }
    if (missing(path) || is.null(path)) {
        stop("A path is required.")
    }

    vault = Vault.retrieve(id)

    if (substring(path, 1, 1) != '/') {
        # Add missing / to path
        path = paste('/', path, sep='')
    }

    parts <- strsplit(path, split='/', fixed=TRUE)[[1]]
    # Get the final folder name from the last part of the path
    folder_name = parts[[length(parts)]]
    # Remove the folder name from the path
    parents = utils::head(parts, -1)

    if (recursive) {
        current_dir = ''
        parent_object_id = NULL

        # Check for existing objects, and ensure they are folders.
        # Create directories as needed
        for (dir in parents) {
            if (dir == '') {
                next
            }

            current_dir = paste(current_dir, dir, sep='/')
            obj = Object.get_by_path(path=current_dir, vault_id=vault$id)

            if (is.null(obj) || (is.data.frame(obj) && nrow(obj) == 0)) {
                obj = Object.create(
                              vault_id=vault$id,
                              parent_object_id=parent_object_id,
                              object_type='folder',
                              filename=dir
                              )
            }

            if (obj$object_type != 'folder') {
                stop(sprintf("Invalid path: existing object at '%s' is not a folder\n", current_dir))
            }

            parent_object_id = obj$id
        }
    }
    else {
        # Find the parent object (folder) at the provided path
        parent_path = paste(parents, collapse="/")
        parent_object = Object.get_by_path(parent_path, vault_id=vault$id)
        if (is.null(parent_object) || parent_object$object_type != 'folder') {
            stop(sprintf("Invalid path: existing object at '%s' is not a folder\n", parent_object))
        }
        parent_object_id = parent_object$id
    }

    object = Object.create(
                   vault_id=vault$id,
                   parent_object_id=parent_object_id,
                   object_type='folder',
                   filename=folder_name,
                   ...)

    return(object)
}


#
# Vault Private Methods
#

# Retrieves objects within a specific vault.
.object_list_helper = function(id, ...) {
    objects = Object.all(vault_id=id, ...)
    return(objects$data)
}
