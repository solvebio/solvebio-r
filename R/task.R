#' Task.all
#'
#' Retrieves the metadata about all tasks on SolveBio accessible to the current user.
#'
#' @param env (optional) Custom client environment.
#' @param ... (optional) Additional query parameters.
#'
#' @examples \dontrun{
#' Task.all()
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
Task.all <- function(env = solvebio:::.solveEnv, ...) {
    .request('GET', "v2/tasks", query=list(...), env=env)
}

#' Task.retrieve
#'
#' Retrieves the metadata about a specific task on SolveBio.
#'
#' @param id The ID of a task. 
#' @param env (optional) Custom client environment.
#'
#' @examples \dontrun{
#' Task.retrieve("1234567890")
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
Task.retrieve <- function(id, env = solvebio:::.solveEnv) {
    if (missing(id)) {
        stop("A task ID is required.")
    }

    path <- paste("v2/tasks", paste(id), sep="/")
    .request('GET', path=path, env=env)
}


#' Task.follow
#'
#' A helper function to follow a specific tasks.
#'
#' @param id String The ID of a task.
#' @param env (optional) Custom client environment.
#'
#' @examples \dontrun{
#' Task.follow("1234567890")
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
Task.follow <- function(id, env = solvebio:::.solveEnv) {
    imp <- Task.retrieve(id)

    while(imp$status == "pending" || imp$status == "queued" || imp$status == "running") {
        imp <- Task.retrieve(id)
        cat(paste("Task", id, "status:", imp$status, "\n", sep=" "))
        Sys.sleep(4)
    }
}
