# Internal functions and environments used internally
# by the SolveBio package (not exported).

# Store the SolveBio config in an environment
# The default environment uses an API key (token type: "Token").
.solveEnv <- new.env()
.solveEnv$host <- Sys.getenv('SOLVEBIO_API_HOST',
                             unset='https://api.solvebio.com')
.solveEnv$token <- Sys.getenv('SOLVEBIO_API_KEY', unset='')
.solveEnv$token_type <- 'Token'

if (nchar(.solveEnv$token) == 0L) {
    # No API key, look for access token
    .solveEnv$token <- Sys.getenv('SOLVEBIO_ACCESS_TOKEN', unset='')
    if (nchar(.solveEnv$token) > 0L) {
        .solveEnv$token_type <- 'Bearer'
    }
}


#' login
#'
#' Store and verify your SolveBio credentials.
#'
#' @param api_key Your SolveBio API key
#' @param api_host SolveBio API host (default: https://api.solvebio.com)
#' @param env (optional) The R environment used to store API credentials.
#'
#' @examples \dontrun{
#' login()
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
login <- function(api_key, api_host, env = solvebio:::.solveEnv) {
    if (!missing(api_key)) {
        assign('token', api_key, envir=env)
    }
    .solveEnv$token <- Sys.getenv('SOLVEBIO_API_KEY', unset='')
    .solveEnv$token_type <- 'Token'

    if(nchar(env$token) == 0L) {
        # No API key, look for access token
        .solveEnv$token <- Sys.getenv('SOLVEBIO_ACCESS_TOKEN', unset='')
        if (nchar(.solveEnv$token) > 0L) {
            .solveEnv$token_type <- 'Bearer'
        }
        else {
            stop("No Access Token or API key found. Learn more: https://docs.solvebio.com/#authenticating-with-r")
        }
    }
    if (!missing(api_host)) {
        assign('host', api_host, envir=env)
    }

    # Test the login
    tryCatch({
        user <- User.retrieve(env=env)
        cat(sprintf("Logged-in to %s as %s.\n", env$host, user$email))
        return(invisible(user))
    }, error = function(e) {
        cat(sprintf("Login failed: %s\n", e$message))
    })
}


#' createEnv
#'
#' Create a new SolveBio environment.
#'
#' @param token A SolveBio API key or OAuth2 token
#' @param token_type SolveBio token type (default: Token)
#' @param host (optional) The SolveBio API host (default: https://api.solvebio.com) 
#'
#' @examples \dontrun{
#' env <- createEnv("MyAPIkey")
#' User.retrieve(env = myEnv)
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
createEnv <- function(token, token_type="Token", host=.solveEnv$host) {
    newEnv <- new.env()
    newEnv$token <- token
    newEnv$token_type <- token_type
    newEnv$host <- host
    return(newEnv)
}


# Private API request method.
.request = function(method, path, query, body, env = solvebio:::.solveEnv, content_type="application/json", ...) {
    'Perform an HTTP request to the server.'
    # Set defaults
    headers <- c(
                 "Accept" = "application/json",
                 "Accept-Encoding" = "gzip,deflate",
                 "Content-Type" = content_type
                 )

    if (!is.null(env$token) && nchar(env$token) != 0) {
        headers <- c(
                     headers, 
                     Authorization = paste(env$token_type, env$token, sep=" ")
                     )
    }

    # Slice of beginning slash
    if (substring(path, 1, 1) == "/") {
        path <- substring(path, 2)
    }
    
    uri <- httr::modify_url(env$host, "path" = path)
    useragent <- sprintf('SolveBio R Client %s [%s %s]',
                         packageVersion('solvebio'),
                         R.version$version.string,
                         R.version$platform)
    config <- httr::config(useragent = useragent)
    encode = "form"

    if (content_type == "application/json") {
        if (!missing(body) && !is.null(body) && length(body) > 0) {
            body <- jsonlite::toJSON(body, auto_unbox=TRUE, null="null")
            encode = "json"
        }
    }

    if (missing(query)) {
        query = NULL
    }

    switch(method, 
           GET={
               res <- httr::GET(
                                uri,
                                httr::add_headers(headers),
                                config = config,
                                query = query,
                                # httr::verbose(),
                                ...
                                )
           },
           POST={
               res <- httr::POST(
                                 uri,
                                 httr::add_headers(headers),
                                 config = config,
                                 body = body,
                                 query = query,
                                 encode = encode,
                                 # httr::verbose(),
                                 ...
                                 )
           },
           PUT={
               res <- httr::PUT(
                                 uri,
                                 httr::add_headers(headers),
                                 config = config,
                                 body = body,
                                 query = query,
                                 encode = encode,
                                 # httr::verbose(),
                                 ...
                                 )
           },
           PATCH={
               res <- httr::PATCH(
                                 uri,
                                 httr::add_headers(headers),
                                 config = config,
                                 body = body,
                                 query = query,
                                 encode = encode,
                                 # httr::verbose(),
                                 ...
                                 )
           },
           DELETE={
               res <- httr::DELETE(
                                 uri,
                                 httr::add_headers(headers),
                                 config = config,
                                 query = query,
                                 # httr::verbose(),
                                 ...
                                 )
           },
           {
               stop('Invalid request method!')
           }
           )


    if (res$status < 200 | res$status >= 400) {
        if (res$status == 429) {
            stop(sprintf("API error: Too many requests, please retry in %i seconds\n", res$header$'retry-after')) 
        }
        if (res$status == 400) {
            tryCatch({
                content = formatSolveBioResponse(res, raw = FALSE)
                if (!is.null(content$detail)) {
                    stop(sprintf("API error: %s\n", content$detail))
                } else {
                    stop(sprintf("API error: %s\n", content))
                }
            }, error = function(e) {
                cat(sprintf("Error parsing API response\n"))
                stop(res)
            })
        }
        if (res$status == 401) {
            stop(sprintf("Unauthorized: %s (error %s)\n", httr::content(res, as="parsed")$detail, res$status))
        }

        content = httr::content(res, as="text", encoding="UTF-8")
        stop(sprintf("API error %s %s\n", res$status, content))
    }

    if (res$status == 204 | res$status == 301 | res$status == 302) {
        return(res)
    }

    res = formatSolveBioResponse(res, raw = FALSE)

    if (!is.null(res$class_name)) {
        # Classify the result object
        return(structure(res, class=res$class_name))
    } else {
        return(res)
    }
}
