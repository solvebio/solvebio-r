# Internal functions and environments used internally
# by the SolveBio package (not exported).

# Store the SolveBio config in an environment
.solveEnv <- new.env()
.solveEnv$current <- .solveEnv
.solveEnv$current$api_host <- Sys.getenv('SOLVEBIO_API_HOST',
                                         unset='https://api.solvebio.com')
.solveEnv$current$api_key <- Sys.getenv('SOLVEBIO_API_KEY', unset='')

#' login
#'
#' Store and verify your SolveBio credentials.
#'
#' @param api_key Your SolveBio API key
#' @param api_host SolveBio API host (default: https://api.solvebio.com)
#' @param envir (optional) The R environment used to store API credentials.
#'
#' @examples \dontrun{
#' login()
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
login <- function(api_key, api_host, envir = solvebio:::.solveEnv$current) {
    if (!missing(api_key)) {
        assign('api_key', api_key, envir=envir)
    }

    if(nchar(envir$api_key) == 0L) {
        stop("No API key found. Please set the 'SOLVEBIO_API_KEY' environment variable, or specify your key as the 'api_key' parameter of this function. Your API key can be found on the Account page of the SolveBio website: https://my.solvebio.com/settings/security")
    }

    if (!missing(api_host)) {
        assign('api_host', api_host, envir=envir)
    }

    # Test the login
    tryCatch({
        res <- .request('GET', 'v1/user')
        cat(sprintf("Logged-in to %s as %s.\n", envir$api_host, res$email))
        return(invisible(res))
    }, error = function(e) {
        cat(sprintf("Login failed: %s\n", e$message))
    })
}

.request = function(method, path, query, body, ...) {
    'Perform an HTTP request to the server.'
    env <- .solveEnv$current

    # Set defaults
    headers <- c(
                 Accept = "application/json",
                 "Content-Type" = "application/json",
                 "Accept-Encoding" = "gzip,deflate"
                 )

    if (!is.null(env$api_key) & env$api_key != "") {
        api_key <- env$api_key
        headers <- c(
                     headers, 
                     Authorization = paste("Token", api_key)
                     )
    }

    # Slice of beginning slash
    if (substring(path, 1, 1) == "/") {
        path <- substring(path, 2)
    }
    
    uri <- httr::modify_url(env$api_host, "path" = path)
    useragent <- sprintf('SolveBio R Client %s [%s %s]',
                         packageVersion('solvebio'),
                         R.version$version.string,
                         R.version$platform)
    config <- httr::config(useragent = useragent)

    if (!missing(body)) {
        body <- jsonlite::toJSON(body, auto_unbox=TRUE)
    } else {
        body = NULL
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
                                 encode = "json",
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
                                 encode = "json",
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
                                 encode = "json",
                                 ...
                                 )
           },
           DELETE={
               res <- httr::DELETE(
                                 uri,
                                 httr::add_headers(headers),
                                 config = config,
                                 query = query,
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
            content = formatSolveBioResponse(res, raw = FALSE)
            if (!is.null(content$detail)) {
                stop(sprintf("API error: %s\n", content$detail)) 
            } else {
                stop(sprintf("API error: %s\n", content)) 
            }
        }
        stop(sprintf("API error: %s\n", res$status)) 
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
