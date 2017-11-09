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
#'                  "abcd1234",
#'                  name="New app name"
#'                 )
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
#' @param description (optional) An optional description for the application.
#' @param env (optional) Custom client environment.
#' @param ... (optional) Additional application attributes.
#'
#' @examples \dontrun{
#' Application.create(
#'                  title="My new application",
#'                  )
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
Application.create <- function(name, description, env = solvebio:::.solveEnv, ...) {
    if (missing(name)) {
        stop("A name is required.")
    }
    if (missing(description)) {
        description = ""
    }

    params = list(
                  title=title,
                  description=description,
                  ...
                  )

    .request('POST', path='v2/applications', query=NULL, body=params, env=env)
}


#' Application.shinyServer
#'
#' Wraps an existing Shiny server in an OAuth2 flow.
#'
#' @param client_id The application's client ID.
#' @param server The Shiny server function.
#' @param base_url (optional) Override the default login host (default: https://my.solvebio.com).
#'
#' @examples \dontrun{
#' Application.shinyServer(
#'                         client_id="abcd1234",
#'                         server=server,
#'                        )
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
Application.shinyServer <- function(client_id, server, base_url) {
    if (missing(base_url)) {
        # Can be used to override the login URL.
        base_url = "https://my.solvebio.com"
    }

    # Return a wrapped Shiny server function
    function(input, output, session, ...) {

        # OAuth2 helper functions
        .makeAuthorizationURL <- function(client_id, redirect_uri, base_url) {
            url <- "%s/authorize?client_id=%s&redirect_uri=%s&response_type=code"
            sprintf(url,
                    base_url,
                    utils::URLencode(client_id, reserved = TRUE, repeated = TRUE),
                    utils::URLencode(redirect_uri, reserved = TRUE, repeated = TRUE)
                    )
        }

        .makeAppURL <- function(session) {
            port <- session$clientData$url_port
            url <- paste0(session$clientData$url_protocol,
                          "//",
                          session$clientData$url_hostname,
                          if(port != "") paste0(":", port),
                          session$clientData$url_pathname)
        }

        .shinyLoginModal <- function(authorization_url) {
            onclick <- sprintf("window.location = '%s'", authorization_url)
            modalDialog(
                        span('Please log in with SolveBio before proceeding.'),
                        footer = tagList(
                                         shiny::actionButton(inputId='login-button',
                                                             label="Log in with SolveBio",
                                                             onclick=onclick)
                                         ),
                        easyClose = FALSE
                        )
        }

        observeEvent(session$clientData$url_search, {
                         params <- gsub(pattern = "?", replacement = "", x = session$clientData$url_search)
                         parsed_params <- parseQueryString(params)
                         # Remove the code from the query params after parsing
                         shiny::updateQueryString("?", mode="replace", session)
                         redirect_uri <- .makeAppURL(session)

                         if (is.null(parsed_params$code)) {
                             authorization_url <- .makeAuthorizationURL(client_id, redirect_uri, base_url)
                             session$userData$access_token <- NULL
                             showModal(.shinyLoginModal(authorization_url))
                         }
                         else {
                             # Retrieve an access_token from the code
                             oauth_params <- list(
                                                  client_id=client_id,
                                                  grant_type="authorization_code",
                                                  redirect_uri=redirect_uri,
                                                  code=parsed_params$code
                                                  )
                             oauth_data <- .request("POST", path="v1/oauth2/token", query=NULL, body=oauth_params, content_type="application/x-www-form-urlencoded")
                             session$userData$solvebio_env <- solvebio::createEnv(token=oauth_data$access_token, token_type="Bearer")

                             # Run the wrapped server
                             server(input, output, session, ...)
                         }
                    }, once = TRUE)
    }
}
