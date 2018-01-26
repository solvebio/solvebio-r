#' @importFrom utils modifyList packageVersion

formatSolveBioResponse <- function (res, raw = FALSE) {
    url = res$url
    body = httr::content(res, as="text", encoding="UTF-8")
    if (raw) {
        return(body)
    } else {
        # Only simplify uniform JSON dictionary responses
        # as data frames. These responses include
        # resource GET requests (list and detail)
        # as well as dataset queries.
        res = jsonlite::fromJSON(body,
                                 simplifyVector = FALSE,
                                 simplifyDataFrame = TRUE,
                                 simplifyMatrix = FALSE)
        res$'_url' = url
        return(res)
    }
}

formatSolveBioQueryResponse <- function (res, raw = FALSE) {
    # res will be the output of formatSolveBioResponse
    if (!raw & is.data.frame(res$results)) {
        # Flatten the data frame
        res$results <- jsonlite::flatten(res$results)
    } else {
        res$results <- as.data.frame(res$results)
    }
    return(res)
}

prepareArgs <- function (args) {
    if (!is.null(args) && length(args) > 0) {
        args = args[!sapply(args, is.null)]
        rapply(args, function (x) {
                   ifelse(is.logical(x), x * 1, x)
                                 }, how="replace")
    } else {
        NULL
    }
}
