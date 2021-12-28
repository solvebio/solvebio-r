#' GlobalSearch.search
#'
#' Performs Global Search based on provided filters.
#' @param filters (optional) Query filters.
#' @param env (optional) Custom client environment.
#' @param ... (optional) Additional query parameters (e.g. limit, offset).
#'
#' @examples \dontrun{
#' # No filters are applied
#' GlobalSearch.search()
#'
#' # Global Beacon search
#' GlobalSearch.search('{"entities":[["gene","BRCA2"]]}')
#'
#' # Type filter (only vaults)
#' GlobalSearch.search('{"filters":[{"and":[["type__in",["vault"]]]}]}')
#'
#' # Advanced search
#' GlobalSearch.search('{"filters":[{"and":[{"and":[["created_at__range",["2020-12-27","2021-12-27"]]]}]}],"query":"fuji"}')
#'
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
GlobalSearch.search <- function(filters, env = solvebio:::.solveEnv, ...) {
  body = list(...)
  # Filters can be passed as a JSON string
  if (!missing(filters) && !is.null(filters) && length(filters) > 0) {
    if (class(filters) == "character") {
      # Convert JSON string to an R structure
      filters <- jsonlite::fromJSON(filters,
                                    simplifyVector = FALSE,
                                    simplifyDataFrame = TRUE,
                                    simplifyMatrix = FALSE)

    }
    # Add filters to request body
    body = modifyList(body, filters)
  }



  tryCatch({
    res <- .request('POST', path="v2/search", body=body, env=env)
    res <- formatSolveBioQueryResponse(res)

    return(res)
  }, error = function(e) {
    cat(sprintf("Query failed: %s\n", e$message))
  })
}
