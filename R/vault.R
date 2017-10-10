#' Vault.all
#'
#' Retrieves the metadata about all accessible vaults.
#'
#' @param env (optional) Custom client environment.
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
Vault.all <- function(..., env = solvebio:::.solveEnv) {
    .request("GET", "v2/vaults", query=list(...), env=env)
}


#' Vault.retrieve
#'
#' Retrieves the metadata about a specific SolveBio vault.
#'
#' @param id String The ID of a SolveBio vault
#' @param env (optional) Custom client environment.
#'
#' @examples \dontrun{
#' Vault.retrieve("1234567890")
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
Vault.retrieve <- function(id, env = solvebio:::.solveEnv) {
    if (missing(id)) {
        stop("A vault ID is required.")
    }

    path <- paste("v2/vaults", paste(id), sep="/")
    .request("GET", path=path, env=env)
}


#' Vault.delete
#'
#' Delete a specific vault from SolveBio. This operation cannot be undone.
#'
#' @param id String The ID of a SolveBio vault.
#' @param env (optional) Custom client environment.
#'
#' @examples \dontrun{
#' Vault.delete("1")
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
Vault.delete <- function(id, env = solvebio:::.solveEnv) {
    if (missing(id)) {
        stop("A vault ID is required.")
    }

    path <- paste("v2/vaults", paste(id), sep="/")
    .request("DELETE", path=path, env=env)
}


#' Vault.create
#'
#' Create a new SolveBio vault.
#' @param name The unique name of the vault.
#' @param env (optional) Custom client environment.
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
Vault.create <- function(name, env = solvebio:::.solveEnv, ...) {
    # TODO
    if (missing(name)) {
        stop("A name is required.")
    }

    params = list(
                  name=name,
                  ...
                  )

    vault <- .request("POST", path="v2/vaults", query=NULL, body=params, env=env)

    return(vault)
}


#' Vault.update
#'
#' Updates the attributes of an existing vault.
#'
#' @param id The ID of the vault to update.
#' @param env (optional) Custom client environment.
#' @param ... Vault attributes to change.
#'
#' @examples \dontrun{
#' Vault.update(
#'              id="1234",
#'              name="New Vault Name",
#'             )
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
Vault.update <- function(id, env = solvebio:::.solveEnv, ...) {
    if (missing(id)) {
        stop("A vault ID is required.")
    }

    params = list(...)

    path <- paste("v2/vaults", paste(id), sep="/")
    .request('PATCH', path=path, query=NULL, body=params, env=env)
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
#' @param env (optional) Custom client environment.
#'
#' @examples \dontrun{
#' Vault.get_by_full_path("SolveBio:Public")
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
Vault.get_by_full_path <- function(full_path, verbose=TRUE, env = solvebio:::.solveEnv) {
    if (missing(full_path)) {
        stop("A vault full path is required.")
    }

    split_path = strsplit(full_path, ":", fixed=TRUE)[[1]]

    if (length(split_path) == 1) {
        # Get the user"s account for them
        user = User.retrieve(env=env)
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
    response = .request("GET", path="v2/vaults", query=params, env=env)

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
#' @param env (optional) Custom client environment.
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
Vault.get_or_create_by_full_path <- function(full_path, env = solvebio:::.solveEnv, ...) {
    vault = Vault.get_by_full_path(full_path, verbose=FALSE, env=env)
    if (!is.null(vault)) {
        # Return if exists
        return(vault)
    }

    split_path = strsplit(full_path, ":")[[1]]
    name = split_path[length(split_path)]
    vault = Vault.create(name=name, env=env, ...)

    return(vault)
}


#' Vault.get_personal_vault
#'
#' Retrieves the current users"s personal, private vault.
#'
#' @param env (optional) Custom client environment.
#'
#' @examples \dontrun{
#' Vault.get_personal_vault()
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
Vault.get_personal_vault <- function(env = solvebio:::.solveEnv) {
    user = User.retrieve(env=env)
    params = list(
                  name=paste("user", user$id, sep="-"),
                  vault_type="user"
                  )

    response = .request("GET", path="v2/vaults", query=params, env=env)

    return(response$data[1, ])
}


#' Vault.files
#'
#' Retrieves all files in a specific vault.
#'
#' @param id The ID of the vault.
#' @param env (optional) Custom client environment.
#' @param ... (optional) Additional query parameters (e.g. limit, offset).
#'
#' @examples \dontrun{
#' vault = Vault.get_personal_vault()
#' Vault.files(vault$id)
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
Vault.files <- function(id, env = solvebio:::.solveEnv, ...) {
    objects = .object_list_helper(id, object_type="file", env=env, ...)
    return(objects)
}


#' Vault.folders
#'
#' Retrieves all folders in a specific vault.
#'
#' @param id The ID of the vault.
#' @param env (optional) Custom client environment.
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
Vault.folders <- function(id, env = solvebio:::.solveEnv, ...) {
    objects = .object_list_helper(id, object_type="folder", env=env, ...)
    return(objects)
}


#' Vault.datasets
#'
#' Retrieves all datasets in a specific vault.
#'
#' @param id The ID of the vault.
#' @param env (optional) Custom client environment.
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
Vault.datasets <- function(id, env = solvebio:::.solveEnv, ...) {
    objects = .object_list_helper(id, object_type="dataset", env=env, ...)
    return(objects)
}


#' Vault.objects
#'
#' Retrieves all objects in a specific vault.
#'
#' @param id The ID of the vault.
#' @param env (optional) Custom client environment.
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
Vault.objects <- function(id, env = solvebio:::.solveEnv, ...) {
    objects = .object_list_helper(id, env=env, ...)
    return(objects)
}


#' Vault.search
#'
#' Search for objects in a specific vault.
#'
#' @param id The ID of the vault.
#' @param query The search query.
#' @param env (optional) Custom client environment.
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
Vault.search <- function(id, query, env = solvebio:::.solveEnv, ...) {
    objects = .object_list_helper(id, query=query, env=env, ...)
    return(objects)
}


#' Vault.create_dataset
#'
#' Create a new dataset in a vault.
#'
#' @param id The ID of the vault.
#' @param path The path to the dataset, within the vault.
#' @param name The name (filename) for the dataset.
#' @param env (optional) Custom client environment.
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
Vault.create_dataset <- function(id, path, name, env = solvebio:::.solveEnv, ...) {
    if (missing(id)) {
        stop("A vault ID is required.")
    }

    vault = Vault.retrieve(id, env=env)

    if (missing(path) || path == "/" || is.null(path)) {
        vault_parent_object_id = NULL
    }
    else {
        user = User.retrieve(env=env)
        account_domain = user$account$domain
        full_path = paste(account_domain, vault$name, path, sep=":")
        # Find the parent object (folder) at the provided path
        parent_object = Object.get_by_full_path(full_path, env=env)
        vault_parent_object_id = parent_object$id
    }

    dataset = Dataset.create(
                   vault_id=vault$id,
                   vault_parent_object_id=vault_parent_object_id,
                   name=name,
                   env=env,
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
#' @param env (optional) Custom client environment.
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
Vault.create_folder <- function(id, path, recursive=FALSE, env = solvebio:::.solveEnv, ...) {
    if (missing(id) || is.null(id)) {
        stop("A vault ID is required.")
    }
    if (missing(path) || is.null(path)) {
        stop("A path is required.")
    }

    vault = Vault.retrieve(id, env=env)

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
            obj = Object.get_by_path(path=current_dir, vault_id=vault$id, env=env)

            if (is.null(obj) || (is.data.frame(obj) && nrow(obj) == 0)) {
                obj = Object.create(
                              vault_id=vault$id,
                              parent_object_id=parent_object_id,
                              object_type='folder',
                              filename=dir,
                              env=env
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
        parent_object = Object.get_by_path(parent_path, vault_id=vault$id, env=env)
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
                   env=env,
                   ...)

    return(object)
}


#
# Vault Private Methods
#

# Retrieves objects within a specific vault.
.object_list_helper = function(id, env = solvebio:::.solveEnv, ...) {
    objects = Object.all(vault_id=id, env=env, ...)
    return(objects$data)
}
