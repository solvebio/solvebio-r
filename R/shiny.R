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
protectedServer <- function(server, client_id, client_secret=NULL, base_url="https://my.solvebio.com") {
    if (!requireNamespace("shiny", quietly = TRUE)) {
        stop("Shiny is required to use solvebio::protectedServer()")
    }

    # Initialize the server session using a SolveBio token.
    # Sets up the SolveBio env and current user.
    .initializeSession <- function(session, token) {
        if (is.null(token)) {
            env <- NULL
            user <- NULL
        }
        else {
            env <- solvebio::createEnv(token=token, token_type="Bearer")
            user <- User.retrieve(env=env)
        }

        session$userData$solvebio_env <- env
        session$userData$solvebio_user <- user

        session
    }

    # OAuth2 helper functions
    .makeRedirectURI <- function(session) {
        port <- session$clientData$url_port
        url <- paste0(session$clientData$url_protocol,
                      "//",
                      session$clientData$url_hostname,
                      if(port != "") paste0(":", port),
                      session$clientData$url_pathname)
    }

    .makeAuthorizationURL <- function(session) {
        url <- "%s/authorize?client_id=%s&redirect_uri=%s&response_type=code"
        sprintf(url,
                base_url,
                utils::URLencode(client_id, reserved = TRUE, repeated = TRUE),
                utils::URLencode(.makeRedirectURI(session), reserved = TRUE, repeated = TRUE)
                )
    }

    .showLoginModal <- function(session) {
        onclick <- sprintf("window.location = '%s'", .makeAuthorizationURL(session))
        modal <- shiny::modalDialog(
                                    shiny::tags$span('Please log in with SolveBio before proceeding.'),
                                    footer = shiny::tagList(
                                                            shiny::actionButton(inputId='login-button',
                                                                                label="Log in with SolveBio",
                                                                                onclick=onclick)
                                                            ),
                                    easyClose = FALSE
                                    )
        shiny::showModal(modal)
    }

    # By default cookie auth is disabled, set up dummy functions.
    # They will be overwritten below if encryption is enabled.
    .encryptToken <- function(token) {
        token
    }

    .decryptToken <- function(token) {
        token
    }

    # Return a wrapped Shiny server function
    function(input, output, session, ...) {

        enable_cookie_auth <- tryCatch({
            if (requireNamespace("shinyjs", quietly = TRUE)) {
                # This will fail if getCookie is not declared in JS
                shinyjs::js$enableCookieAuth()
                TRUE
            }
            else {
                warning("WARNING: This app requires shinyjs to use cookies for token storage.")
                FALSE
            }
        }, error = function(e) {
            # Cookie JS is not enabled, disable cookies 
            warning("WARNING: This app has not been configured to use cookies for token storage.")
            return(FALSE)
        })

        if (enable_cookie_auth) {
            # Setup token encryption using the secret key
            if (!is.null(client_secret) && client_secret != "") {
                requireNamespace("digest")
                requireNamespace("PKI")

                aes_key <- substr(digest::sha1(client_secret), 0, 32)
                # Use ECB mode to support restarts of the Shiny server
                # without deactivating existing encrypted keys. 
                aes <- digest::AES(charToRaw(aes_key), mode="ECB")

                .encryptToken <- function(token) {
                    # Pad the token and convert to hex string
                    raw <- charToRaw(token)
                    raw <- c(raw, as.raw(rep(0, 16 - length(raw) %% 16)))
                    encrypted_raw <- aes$encrypt(raw)
                    paste(PKI::raw2hex(encrypted_raw), collapse = '')
                }

                .decryptToken <- function(encrypted_token) {
                    # Convert hex string to raw for decryption
                    hex <- strsplit(encrypted_token, character(0))[[1]]
                    hex <- paste(hex[c(TRUE, FALSE)], hex[c(FALSE, TRUE)], sep = "")
                    encrypted_raw <- as.raw(as.hexmode(hex))
                    raw <- aes$decrypt(encrypted_raw, raw=TRUE)
                    rawToChar(raw[raw>0])
                }
            }
            else {
                warning("WARNING: SolveBio OAuth2 tokens will not be encrypted in cookies. Set client_secret to encrypt tokens.")
            }

            shiny::observe({
                try(shinyjs::js$getCookie(), silent = FALSE)

                # Only proceed if the token cookie is set
                shiny::req(input$tokenCookie)

                # Handle the case where an auth cookie is found
                tryCatch({
                    # Setup and test the auth token
                    .initializeSession(session, token=.decryptToken(input$tokenCookie))
                    # Close the auth modal which will be opened by the code below
                    shiny::removeModal()
                }, error = function(e) {
                    # Cookie has an invalid/expired token.
                    # Clear the cookie and show the auth modal.
                    try(shinyjs::js$rmCookie())
                    session$reload()
                })

                # Run the wrapped server
                server(input, output, session, ...)
            })

            shiny::observeEvent(input$logout,
                                {
                                    try(shinyjs::js$rmCookie())
                                    session$reload()
                                })
        }
        else {
            warning("WARNING: SolveBio cookie-based token storage is disabled.")
        }

        shiny::observeEvent(session$clientData$url_search, {
                                params <- gsub(pattern = "?", replacement = "", x = session$clientData$url_search)
                                parsed_params <- shiny::parseQueryString(params)

                                if (!is.null(parsed_params$code)) {
                                    # Remove the code from the query params after parsing
                                    # NOTE: Setting updateQueryString to an empty string or relative path
                                    #       causes browsers to prepend the "base href" string
                                    #       which contains a session identifier on Shiny Server Pro.
                                    redirect_uri <- .makeRedirectURI(session)
                                    shiny::updateQueryString(redirect_uri, mode="replace")
                                    # Retrieve an access_token from the code
                                    oauth_params <- list(
                                                         client_id=client_id,
                                                         client_secret=client_secret,
                                                         grant_type="authorization_code",
                                                         redirect_uri=redirect_uri,
                                                         code=parsed_params$code
                                                         )
                                    oauth_data <- tryCatch({
                                        .request("POST",
                                                 path="v1/oauth2/token",
                                                 query=NULL,
                                                 body=oauth_params,
                                                 content_type="application/x-www-form-urlencoded")
                                    }, error = function(e) {
                                        stop("ERROR: Unable to retrieve SolveBio OAuth2 token. Check your client_id and client_secret (if used).")
                                    })

                                    # Set an auth cookie using a JS cookie library
                                    if (enable_cookie_auth) {
                                        # Setting the cookie will run the server above
                                        shinyjs::js$setCookie(.encryptToken(oauth_data$access_token))
                                    }
                                    else {
                                        # Set the token and retrieve the user
                                        .initializeSession(session, token=oauth_data$access_token)
                                        # Run the wrapped server
                                        server(input, output, session, ...)
                                    }
                                }
                                else {
                                    # Clear the session and show the modal
                                    .initializeSession(session, token=NULL)
                                    .showLoginModal(session)
                                }
            }, once = TRUE)
    }
}


#' protectedServerUI
#'
#' Returns ShinyJS-compatible JS code to support cookie-based token storage.
#'
#' @examples \dontrun{
#' jscookie_src <- "https://cdnjs.cloudflare.com/ajax/libs/js-cookie/2.2.0/js.cookie.js"
#' ui <- fluidPage(
#'     shiny::tags$head(
#'         shiny::tags$script(src = jscookie_src)
#'     ),
#'     useShinyjs(),
#'     extendShinyjs(text = solvebio::protectedServerJS(),
#'                   functions = c("enableCookieAuth", "getCookie", "setCookie", "rmCookie"))
#' )
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
protectedServerJS <- function() {
    if (!requireNamespace("shinyjs", quietly = TRUE)) {
        stop("ShinyJS is required to use solvebio::protectedServerJS()")
    }

    return ('
    shinyjs.enableCookieAuth = function(params) {
        return true;
    }

    shinyjs.getCookie = function(params) {
        Shiny.onInputChange("tokenCookie", Cookies.get("sb_auth") || "");
    }

    shinyjs.setCookie = function(params) {
        Cookies.set("sb_auth", escape(params), { expires: 0.5 });
    }

    shinyjs.rmCookie = function(params) {
        Cookies.remove("sb_auth");
    }
    ')
}
