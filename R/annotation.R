#' Annotator.annotate
#'
#' Annotate a data table/frame with additional fields.
#'
#' @param records The data table or data frame to annotate.
#' @param fields The fields to add.
#' @param include_errors Set to TRUE to include errors in the output (default: FALSE).
#' @param raw Set to TRUE to return the raw response (default: FALSE).
#' @param env (optional) Custom client environment.
#'
#' @examples \dontrun{
#' Annotator.annotate(records=tbl, fields=fields)
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
Annotator.annotate <- function(records, fields, include_errors=FALSE, raw=FALSE, env = .solveEnv) {
    if (missing(records) || missing(fields)) {
        stop("A data table/frame and fields are both required.")
    }

    params <- list(
                   records=records,
                   fields=fields,
                   include_errors=include_errors
                   )

    response <- .request('POST', path='v1/annotate', query=NULL, body=params, env=env)

    if (raw) {
        return(response)
    } else {
        return(response$results)
    }
}

#' Expression.evaluate
#'
#' Evaluate a SolveBio expression.
#'
#' @param expression The SolveBio expression string.
#' @param data_type The data type to cast the expression result (default: string).
#' @param is_list Set to TRUE if the result is expected to be a list (default: FALSE).
#' @param data Variables used in the expression (default: NULL).
#' @param raw Set to TRUE to return the raw response (default: FALSE).
#' @param env (optional) Custom client environment.
#'
#' @examples \dontrun{
#' Expression.evaluate("1 + 1", data_type="integer", is_list=FALSE)
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
Expression.evaluate <- function(expression, data_type="string", is_list=FALSE, data=NULL, raw=FALSE, env = .solveEnv) {
    if (missing(expression)) {
        stop("A SolveBio expression is required.")
    }

    params <- list(
                   expression=expression,
                   data_type=data_type,
                   is_list=is_list,
                   data=data
                   )

    response <- .request('POST', path='v1/evaluate', query=NULL, body=params, env=env)

    if (raw) {
        return(response)
    } else {
        return(response$result)
    }
}
