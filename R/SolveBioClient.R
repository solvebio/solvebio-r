
# SolveBioClient <- function(api_key, api_host = "https://api.solvebio.com") {
#   if (missing(api_key)) {
#     stop("api_key parameter is required.")
#   }
#
#   url = httr::modify_url(api_host, "path" = "v1/user")
#   authorization = paste("Token", api_key)
#   headers <- httr::add_headers(
#     Authorization = authorization,
#     Accept = "application/json",
#     "Content-Type" = "application/json",
#     "Accept-Encoding" = "gzip,deflate",
#     "User-Agent" = "SolveBio R Client"
#   )
#   res <- httr::GET(url, headers)
#   httr::stop_for_status(res)
#
#   if (res$status != 200) {
#     stop()
#   }
#   res = formatSolveBioResult(res, raw = FALSE)
#
#   structure(list("headers" = headers, "api_host" = api_host), class = "solvebio")
# }
.SolveBioClient <- setRefClass("SolveBioClient",
                               fields = list(
                                             api_host = "character", 
                                             api_key = "character",
                                             request_handle = "NULL"
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
                                                      stop("No access token available! Run login() to start the authentication!")
                                                  }
                                                  request_handle <<- httr::handle()
                                                  # Setup the default headers
                                                  request_handle$handle$headers <<- httr::add_headers(
                                                                    Authorization = paste("Token", api_key),
                                                                    Accept = "application/json",
                                                                    "Content-Type" = "application/json",
                                                                    "Accept-Encoding" = "gzip,deflate",
                                                                    "User-Agent" = "SolveBio R Client"
                                                                    )
                                              },
                                              login = function() {
                                                  # login = function(scope = "browse global") {
                                                  if(is.null(request_handle)) {
                                                      connect()
                                                  }
                                                  res <- request('GET', '/v1/user')
                                              },
                                              isAuthenticated = function() {
                                                  if(is.null(request_handle)) {
                                                      connect()
                                                  }

                                                  res <- request('GET', '/v1/user')

                                                  if(res$status != "200") {
                                                      return(FALSE)
                                                  }

                                                  return(TRUE)
                                              }
                                              ) # methods
                        ) # SolveBioClient


.SolveBioClient$methods(request = function(method, path, ...) {
                        'Perform an HTTP request to the server.'

                        if(is.null(request_handle)) {
                            connect()
                        }
                        
                        url = httr::modify_url(api_host, "path" = path)

                        switch(method, 
                               GET={
                                   res <- httr::GET(uri, handle = request_handle, ...)
                               },
                               POST={
                                   res <- httr::POST(uri, handle = request_handle, ...)
                               },
                               PUT={},
                               PATCH={},
                               {
                                   print('Invalid request method!')
                               }
                               )


                        httr::stop_for_status(res)

                        if (res$status != 200) {
                            cat("\n", toJSON(res$body, pretty = TRUE), "\n")
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
#' client <- SolveBioClient(Sys.getenv("SOLVEBIO_API_KEY"))
#' client.login()
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
SolveBioClient <- function(api_key = character(), api_host = character()) {
    client <- .SolveBioClient$new(api_key = api_key, api_host = api_host)
    # client$connect()
    return(client)
}


setMethod("connect", "SolveBioClient", function(x) x$connect())
setMethod("login", "SolveBioClient", function(x) x$login())
setMethod("isAuthenticated", "SolveBioClient", function(x) x$isAuthenticated())
