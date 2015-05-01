# Internal functions and environments used internally
# by the SolveBio package (not exported).

# Store the SolveBio config in an environment
.solveEnv <- new.env()
.solveEnv$current <- .solveEnv
.solveEnv$current$api_host <- 'https://api.solvebio.com'

# .client <- function(e) {
#     if (missing(e)) e = .solveEnv$current
#     if (!exists('handle', envir=e)) {
#         stop('Not logged-in. Please run login()')
#     }
#     e$handle
# }


#' login
#'
#' Store and verify your SolveBio credentials.
#'
#' @param api_key Your SolveBio API key
#' @param api_host SolveBio API host (default: https://api.solvebio.com)
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
    if (missing(api_key)) {
        api_key <- Sys.getenv('SOLVEBIO_API_KEY')
    }
    if(nchar(api_key) == 0L) {
        # TODO: log in manually
        stop("No API key available!")
    }
    assign('api_key', api_key, envir=envir)

    if (missing(api_host)) {
        # See if an API host is in the environment
        if (!is.na(Sys.getenv('SOLVEBIO_API_HOST', unset=NA))) {
            assign('api_host', Sys.getenv('SOLVEBIO_API_HOST'), envir=envir)
        }
    }
    else {
        assign('api_host', api_host, envir=envir)
    }

    # Test the login
    res <- .request('GET', '/v1/user')

    if (is.null(res)) {
        stop("Invalid API key")
    }
}

.request = function(method, path, body = NULL, ...) {
    'Perform an HTTP request to the server.'
    env <- .solveEnv$current

    # Set defaults
    headers <- c(
                 Accept = "application/json",
                 "Content-Type" = "application/json",
                 "Accept-Encoding" = "gzip,deflate"
                 )

    if (exists('api_key', envir=env)) {
        api_key <- env$api_key
        headers <- c(
                     headers, 
                     Authorization = paste("Token", api_key)
                     )
    }
    
    uri <- httr::modify_url(env$api_host, "path" = path)
    config <- httr::config(
                           httpheader = headers,
                           useragent = 'SolveBio R Client'
                           )

    switch(method, 
           GET={
               res <- httr::GET(
                                uri,
                                config = config
                                )
           },
           POST={
               res <- httr::POST(
                                 uri,
                                 config = config,
                                 body = body,
                                 encode = 'json'
                                 )
           },
           PUT={
               res <- httr::PUT(
                                 uri,
                                 config = config,
                                 body = body,
                                 encode = 'json'
                                 )
           },
           PATCH={
               res <- httr::PATCH(
                                 uri,
                                 config = config,
                                 body = body,
                                 encode = 'json'
                                 )
           },
           {
               print('Invalid request method!')
           }
           )


    if (res$status != 200) {
        httr::warn_for_status(res)
        # cat(res$body, "\n")
        return(invisible(res))
    }

    res = formatSolveBioResult(res, raw = FALSE)
    return(res)
}
