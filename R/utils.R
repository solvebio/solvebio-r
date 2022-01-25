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

formatSolveBioQueryResponse <- function (res, raw = FALSE, col.names = NULL) {
    # res will be the output of formatSolveBioResponse
    if (!raw & is.data.frame(res$results) & !is.null(col.names)) {
        res$results <- as.data.frame(res$results, col.names = col.names)
    }
    else {
        if (!raw & is.data.frame(res$results)) {
            # Flatten the data frame
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

formatQueryColumns <- function (id, env, res, use_field_titles) {
    # Retrieve the list of ordered fields
    ds_fields <- do.call(Dataset.fields, list(id, limit=1000, env=env))$data

    # Create a dataframe that maps column names with titles
    if ("_id" %in% names(res)) {
        # Append column "_id" to the list of names and titles because it's always present in result query, but not in names and titles
        col.name.title.map <- data.frame(
            names = c(ds_fields$name, "_id"),
            title = c(ds_fields$title, "_id"),
            stringsAsFactors = FALSE
        )
    }
    else {
        col.name.title.map <- data.frame(
            names = ds_fields$name,
            title = ds_fields$title,
            stringsAsFactors = FALSE
        )
    }
    # Remove all names and titles that are not in the results query
    col.name.title.map <- col.name.title.map[col.name.title.map$names %in% colnames(res), ]

    # Order columns in the dataframe based on list of dataset fields
    res <- res[col.name.title.map$names]

    if (use_field_titles) {
        # Change column names to titles based on the col.name.title.map dataframe
        colnames(res)[match(col.name.title.map[,1], colnames(res))] <- col.name.title.map[,2][match(col.name.title.map[,1], colnames(res))]
        colnames(res) <- make.unique(colnames(res), sep="_")
    }
    return (res)
}
