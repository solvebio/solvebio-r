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

formatSolveBioQueryResponse <- function (id, res, raw = FALSE, col.names = NULL) {
    # res will be the output of formatSolveBioResponse
    if (!raw & is.data.frame(res$results) & !is.null(col.names)) {
        # Remove columns from col.names that are not in the results query
        row.names$data$names [colnames(res$results)]

        # Flatten the data frame and sort columns by col.names + _id field
        res$results <- jsonlite::flatten(res$results)
        res$results <- res$results[, c(col.names$data$names, "_id")]

        # Create a dataframe that maps column names with titles
        names <-  col.names$data$name # get fields name
        titles <-  col.names$data$title # get fields titles

        # Append column "_id" to the list of names and titles because it's always present in result query, but not in names and titles
        col.name.title.map <- data.frame(
            names = c(names, "_id"),
            title = c(titles, "_ID"),
            stringsAsFactors = FALSE
        )

        # Remove all names and titles that are not in the results query
        col.name.title.map <- col.name.title.map[!col.name.title.map$names %in% diff, ]

        # Change column names to titles based on the col.name.title.map dataframe
        colnames(res$results)[match(col.name.title.map[,1], colnames(res$results))] <- col.name.title.map[,2][match(col.name.title.map[,1], colnames(res$results))]
    } else {
        if (!raw & is.data.frame(res$results)) {
            res$results <- jsonlite::flatten(res$results)
        } else {
            res$results <- as.data.frame(res$results)
        }
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
