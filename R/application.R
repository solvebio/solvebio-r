#' Application.all
#'
#' Retrieves the metadata about all application on SolveBio available to the current user.
#'
#' @param env (optional) Custom client environment.
#' @param ... (optional) Additional query parameters.
#'
#' @examples \dontrun{
#' Application.all()
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
Application.all <- function(env = solvebio:::.solveEnv, ...) {
    .request('GET', "v2/applications", query=list(...), env=env)
}


#' Application.retrieve
#'
#' Retrieves the metadata about a specific application SolveBio.
#'
#' @param client_id The client ID for the application.
#' @param env (optional) Custom client environment.
#'
#' @examples \dontrun{
#' Application.retrieve("abcd1234")
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
Application.retrieve <- function(client_id, env = solvebio:::.solveEnv) {
    if (missing(client_id)) {
        stop("A client ID is required.")
    }

    path <- paste("v2/applications", paste(client_id), sep="/")
    .request('GET', path=path, env=env)
}


#' Application.update
#'
#' Updates the attributes of an existing application.
#'
#' @param client_id The client ID for the application.
#' @param env (optional) Custom client environment.
#' @param ... Application attributes to change.
#'
#' @examples \dontrun{
#' Application.update(
#'                    "abcd1234",
#'                    name="New app name"
#'                    )
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
Application.update <- function(client_id, env = solvebio:::.solveEnv, ...) {
    if (missing(client_id)) {
        stop("A client ID is required.")
    }

    params = list(...)

    path <- paste("v2/applications", paste(client_id), sep="/")
    .request('PATCH', path=path, query=NULL, body=params, env=env)
}


#' Application.delete
#'
#' Delete a specific application from SolveBio.
#'
#' @param client_id The client ID for the application.
#' @param env (optional) Custom client environment.
#'
#' @examples \dontrun{
#' Application.delete("abcd1234")
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
Application.delete <- function(client_id, env = solvebio:::.solveEnv) {
    if (missing(client_id)) {
        stop("A client ID is required.")
    }

    path <- paste("v2/applications", paste(client_id), sep="/")
    .request('DELETE', path=path, env=env)
}


#' Application.create
#'
#' Create a new SolveBio application.
#'
#' @param name The name of the application.
#' @param redirect_uris A list of space-separated OAuth2 redirect URIs.
#' @param env (optional) Custom client environment.
#' @param ... (optional) Additional application attributes.
#'
#' @examples \dontrun{
#' Application.create(
#'                    name="My new application",
#'                    redirect_uris="http://localhost:3838/"
#'                    )
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
Application.create <- function(name, redirect_uris, env = solvebio:::.solveEnv, ...) {
    if (missing(name)) {
        stop("A name is required.")
    }
    if (missing(redirect_uris)) {
        stop("A redirect URI is required.")
    }

    params = list(
                  name=name,
                  redirect_uris=redirect_uris,
                  ...
                  )

    .request('POST', path='v2/applications', query=NULL, body=params, env=env)
}
