# Dataset resource

setClass("SolveObject", contains = "VIRTUAL",
         slots = c(class_name = "character", object = "list"))
setClass("ListObject", contains = "VIRTUAL",
         slots = c(items = "list", total = "integer", offset = "integer", limit = "integer"),
         prototype = prototype(items = list(), total = integer(), offset = 0L, limit = integer()))

setClassUnion("SolveObjectORList", c("SolveObject", "ListObject"))
setClass("APIResource", contains = "VIRTUAL", slots = c(data = "SolveObjectORList"))

#' @export
setClass("Dataset",
         contains = "APIResource"
         )

#' @export
all.Dataset <- function(x, ...) {
    return(client$request('GET', '/v1/datasets'))
}

setMethod("all", "Dataset", all.Dataset)
          #     data <- client$request('GET', '/v1/datasets')
          #     return(data)
          # })
