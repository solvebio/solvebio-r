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

formatSolveBioQueryResponse <- function (id, res, raw = FALSE, row.names = NULL) {
    # res will be the output of formatSolveBioResponse
    if (!raw & is.data.frame(res$results)) {
        # Append column "_id" if it's not already there
        if (!"_id" %in% row.names) {
            row.names <- c(row.names, "_id")
        }
        # Remove columns from row.names that are not in the results query
        diff <- setdiff(row.names, colnames(res$results))
        row.names <- row.names [! row.names %in% diff]

        # Flatten the data frame and sort columns by row.names
        res$results <- jsonlite::flatten(res$results)
        res$results <- res$results[, row.names]

        # Replace the column names with column titles
        names <-  do.call(Dataset.fields, list(id, limit=200))$data$name # get fields name
        titles <-  do.call(Dataset.fields, list(id, limit=200))$data$title # get fields titles

        col.name.title.map <- data.frame(
            names = c(names, "_id"),
            title = c(titles, "_ID"),
            stringsAsFactors = FALSE
        )
        # Remove all names and titles that are not in the results query
        col.name.title.map <- col.name.title.map[!col.name.title.map$names %in% diff, ]
        print(col.name.title.map)

        # Change column names to titles based on the col.name.title.map dataframe
        colnames(res$results)[match(col.name.title.map[,1], colnames(res$results))] <- col.name.title.map[,2][match(col.name.title.map[,1], colnames(res$results))]
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
