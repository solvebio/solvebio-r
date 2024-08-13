#' User.retrieve
#'
#' Retrieves information about the current user.
#'
#' @param env (optional) Custom client environment.
#'
#' @examples \dontrun{
#' User.retrieve()
#' }
#'
#' @references
#' \url{https://docs.solvebio.com/}
#'
#' @export
User.retrieve <- function(env = .solveEnv) {
    .request('GET', "v1/user", env=env)
}
