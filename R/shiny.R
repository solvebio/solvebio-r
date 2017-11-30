#' protectedServer
#'
#' Wraps an existing Shiny server in an OAuth2 flow.
#'
#' @param server Your original Shiny server function.
#' @param client_id Your application's client ID.
#' @param client_secret (optional) Your application's client secret.
#' @param base_url (optional) Override the default login host (default: https://my.solvebio.com).
#'
#' @examples \dontrun{
#' protectedServer(
#'                 server=server,
#'                 client_id="abcd1234"
#'                 )
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
protectedServer <- function(server, client_id, client_secret, base_url="https://my.solvebio.com") {
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

        .makeRedirectURL <- function(session) {
            port <- session$clientData$url_port
            url <- paste0(session$clientData$url_protocol,
                          "//",
                          session$clientData$url_hostname,
                          if(port != "") paste0(":", port),
                          session$clientData$url_pathname)
        }

        .shinyLoginModal <- function(authorization_url) {
            onclick <- sprintf("window.location = '%s'", authorization_url)
            shiny::modalDialog(
                               shiny::tags$span('Please log in with SolveBio before proceeding.'),
                               footer = shiny::tagList(
                                                       shiny::actionButton(inputId='login-button',
                                                                           label="Log in with SolveBio",
                                                                           onclick=onclick)
                                                       ),
                               easyClose = FALSE
                               )
        }

        shiny::observeEvent(session$clientData$url_search, {
                         params <- gsub(pattern = "?", replacement = "", x = session$clientData$url_search)
                         parsed_params <- shiny::parseQueryString(params)
                         redirect_uri <- .makeRedirectURL(session)

                         if (is.null(parsed_params$code)) {
                             authorization_url <- .makeAuthorizationURL(client_id, redirect_uri, base_url)
                             session$userData$solvebio_env <- NULL
                             shiny::showModal(.shinyLoginModal(authorization_url))
                         }
                         else {
                             # Remove the code from the query params after parsing
                             # NOTE: Setting updateQueryString to an empty string or relative path
                             #       causes browsers to prepend the "base href" string
                             #       which contains a session identifier on Shiny Server Pro.
                             shiny::updateQueryString(redirect_uri, mode="replace")
                             # Retrieve an access_token from the code
                             oauth_params <- list(
                                                  client_id=client_id,
                                                  client_secret=client_secret,
                                                  grant_type="authorization_code",
                                                  redirect_uri=redirect_uri,
                                                  code=parsed_params$code
                                                  )
                             oauth_data <- .request("POST", path="v1/oauth2/token", query=NULL, body=oauth_params, content_type="application/x-www-form-urlencoded")
                             session$userData$solvebio_env <- solvebio::createEnv(token=oauth_data$access_token, token_type="Bearer")
                             # TODO: Set an auth cookie using a JS cookie library

                             # Run the wrapped server
                             server(input, output, session, ...)
                         }
                    }, once = TRUE)
    }
}
