.SolveBioClient <- setRefClass("SolveBioClient",
                               fields = list(
                                             api_host = "character", 
                                             api_key = "character",
                                             # httr classes
                                             request_handle = "ANY",
                                             config = "ANY"
                                             ),
                               methods = list(
                                              show = function() {
                                                  'Method for printing the SolveBioClient class.'
                                                  cat("Object of class", classLabel(class(.self)), "with:\n")
                                                  cat("\nAPI Host:      ", methods::show(api_host), "\n")
                                                  cat("\nAPI Key:      ", api_key, "\n")
                                                  cat("\nAuthenticated:     ", (length(api_key) > 0L) && isAuthenticated(), "\n")
                                              },
                                              connect = function() {
                                                  'Sets the request handler.'
                                                  if(length(api_key) == 0L) {
                                                      stop("No API key available! Run login() to start the authentication!")
                                                  }
                                                  request_handle <<- httr::handle(api_host)
                                                  # Setup the default headers
                                                  headers <- c(
                                                               Authorization = paste("Token", api_key),
                                                               Accept = "application/json",
                                                               "Content-Type" = "application/json",
                                                               "Accept-Encoding" = "gzip,deflate"
                                                               )
                                                  config <<- httr::config(
                                                                          httpheader = headers,
                                                                          useragent = 'SolveBio R Client'
                                                                          )
                                              },
                                              # login = function() {
                                              #     # login = function(scope = "browse global") {
                                              #     if(is.null(request_handle)) {
                                              #         connect()
                                              #     }
                                              #     res <- request('GET', '/v1/user')
                                              # },
                                              isAuthenticated = function() {
                                                  if(is.null(request_handle)) {
                                                      connect()
                                                  }

                                                  uri <- httr::modify_url(api_host, "path" = '/v1/user')
                                                  res <- httr::GET(
                                                                   uri,
                                                                   config,
                                                                   handle = request_handle)

                                                  if (res$status != 200) {
                                                      # httr::stop_for_status(res)
                                                      return(FALSE)
                                                  }

                                                  return(TRUE)
                                              }
                                              ) # methods
                        ) # SolveBioClient


.SolveBioClient$methods(request = function(method, path, body = NULL, ...) {
                        'Perform an HTTP request to the server.'

                        if(is.null(request_handle)) {
                            connect()
                        }
                        
                        uri <- httr::modify_url(api_host, "path" = path)

                        switch(method, 
                               GET={
                                   res <- httr::GET(
                                                    uri,
                                                    config = config,
                                                    handle = request_handle)
                               },
                               POST={
                                   res <- httr::POST(
                                                     uri,
                                                     config = config,
                                                     handle = request_handle,
                                                     body = body,
                                                     encode = 'json')
                               },
                               PUT={},
                               PATCH={},
                               {
                                   print('Invalid request method!')
                               }
                               )


                        if (res$status != 200) {
                            cat("\n", toJSON(res$body, pretty = TRUE), "\n")
                            # httr::stop_for_status(res)
                            return(invisible(NULL))
                        }

                        res = formatSolveBioResult(res, raw = FALSE)
                        return(res)
                        })
  

#' SolveBioClient
#'
#' Create an SolveBio client instance
#'
#' @param api_key Your SolveBio API key
#' @param api_host SolveBio API host (default: https://api.solvebio.com)
#'
#' @examples
#' client <- SolveBioClient()
#' client$isAuthenticated()
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
SolveBioClient <- function(api_key = character(), api_host = character()) {
    if (missing(api_host)) {
        api_host <- Sys.getenv('SOLVEBIO_API_HOST', unset='https://api.solvebio.com/')
    }
    if (missing(api_key)) {
        api_key <- Sys.getenv('SOLVEBIO_API_KEY', unset=NA)
    }
    client <- .SolveBioClient$new(api_key = api_key, api_host = api_host)
    client$connect()
    return(client)
}


# setMethod("login", "SolveBioClient", function(x) x$login())
setMethod("connect", "SolveBioClient", function(x) x$connect())
setMethod("isAuthenticated", "SolveBioClient", function(x) x$isAuthenticated())
