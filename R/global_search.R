#' GlobalSearch.request
#'
#' Performs low-level Global Search based on the provided filters, queries and entities.
#' Returns full API response (containing attributes: results, vaults, subjects, subjects_count, total, took and offset)
#'
#' @param query (optional) Advanced search query.
#' @param filters (optional) Low-level filter specification.
#' @param entities (optional) Low-level entity specification.
#' @param env (optional) Custom client environment.
#' @param ... (optional) Additional query parameters (e.g. limit, offset).
#'
#' @examples \dontrun{
#' # No filters are applied
#' GlobalSearch.request()
#'
#' # Global Beacon search
#' GlobalSearch.request(entities = '[["gene","BRCA2"]]')
#'
#' # Type filter (only vaults)
#' GlobalSearch.request(filters = '[{"and":[["type__in",["vault"]]]}]')
#'
#' # Advanced search
#' GlobalSearch.request(query = "fuji")
#'
#'
#' # Multiple filters and entities
#' GlobalSearch.request(
#'   entities = '[["gene","BRCA2"]]',
#'   filters = '[{
#'                "and": [
#'                       {"and": [
#'                          ["created_at__range",["2021-11-28","2021-12-28"]]]},
#'                          ["type__in",["dataset"]]
#'                      ]
#'              }]'
#' )
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
GlobalSearch.request <- function(query=NULL, filters, entities, env = solvebio:::.solveEnv, ...) {
  body = list(...)

  # Advanced search query
  body = modifyList(body, list(query=query))

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
    body = modifyList(body, list(filters=filters))
  }

  # Entities can be passed as a JSON string
  if (!missing(entities) && !is.null(entities) && length(entities) > 0){
    if (class(entities) == "character") {
      # Convert JSON string to an R structure
      entities <- jsonlite::fromJSON(entities,
                                    simplifyVector = FALSE,
                                    simplifyDataFrame = TRUE,
                                    simplifyMatrix = FALSE)
    }
    # Add entities to request body
    body = modifyList(body, list(entities=entities))
  }


  tryCatch({
    res <- .request('POST', path="v2/search", body=body, env=env)
    res <- formatSolveBioQueryResponse(res)

    return(res)
  }, error = function(e) {
    cat(sprintf("Query failed: %s\n", e$message))
  })
}


#' GlobalSearch.search
#'
#' Performs a Global Search based on provided filters, entities, queries, and returns an R data frame containing results from API response.
#' Returns a single page of results otherwise (default).
#'
#' @param paginate When set to TRUE, retrieves all records (memory permitting).
#' @param env (optional) Custom client environment.
#' @param ... (optional) Additional query parameters (e.g. filters, entities, query, limit, offset).
#'
#' @examples \dontrun{
#' # No filters applied
#' GlobalSearch.search()
#'
#' #Global Beacon search
#' GlobalSearch.search(entities = '[["gene","BRCA2"]]')
#'
#  #Type filter (only vaults)
#' GlobalSearch.search(filters = '[{"and":[["type__in",["vault"]]]}]')
#'
#' # Advanced search
#' GlobalSearch.search(query = "fuji")
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
GlobalSearch.search <- function(paginate=FALSE, env = solvebio:::.solveEnv, ...) {
  params <- list(...)
  params$env <- env

  # Retrieve the first page of results
  response <- do.call(GlobalSearch.request, params)
  df <- response$result
  offset <- response$offset

  if (response$total > 100000 && isTRUE(paginate)) {
    warning(paste("This query will retrieve ", response$total, " records, which may take some time..."), call. = FALSE)
  }

  # Continue to make requests for data if pagination is enabled and there are more records
  while (isTRUE(paginate) && !is.null(offset)) {
    params$offset <- offset
    response <- do.call(GlobalSearch.request, params)
    df_page <- response$results
    df <- dplyr::bind_rows(df, df_page)
    offset <- response$offset
  }

  if (!isTRUE(paginate) && !is.null(offset)) {
    warning(paste("This call returned only the first page of records. To retrieve more pages automatically,",
                  "please set paginate=TRUE when calling GlobalSearch.search().", call. = FALSE))
  }


  return(df)
}


#' GlobalSearch.subjects
#'
#' Performs a Global Search based on provided filters, entities, queries, and returns an R data frame containing subjects from API response.
#' @param env (optional) Custom client environment.
#' @param ... (optional) Additional query parameters (e.g. filters, entities, query, limit, offset).
#'
#' @examples \dontrun{
#' GlobalSearch.subjects(entities = '[["gene","BRCA2"]]')
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
GlobalSearch.subjects <- function(env = solvebio:::.solveEnv, ...) {
  params <- list(...)
  params$env <- env

  response <- do.call(GlobalSearch.request, params)
  df <- response$subjects

  return(df)
}
