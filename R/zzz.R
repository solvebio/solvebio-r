.onLoad <- function(libname = find.package("solvebio"), pkgname = "solvebio") {
    .solveEnv$host <- Sys.getenv('SOLVEBIO_API_HOST',
                                 unset='https://api.solvebio.com')
    .solveEnv$token <- Sys.getenv('SOLVEBIO_API_KEY', unset='')
    .solveEnv$token_type <- 'Token'

    if (nchar(.solveEnv$token) == 0L) {
        # No API key, look for access token
        .solveEnv$token <- Sys.getenv('SOLVEBIO_ACCESS_TOKEN', unset='')
        if (nchar(.solveEnv$token) > 0L) {
            .solveEnv$token_type <- 'Bearer'
        }
    }
}

.onAttach <- function(libname = find.package("solvebio"), pkgname = "solvebio") {
    deprecate_msg <- paste(
      "!!! Deprecation Notice",
      "The SolveBio R client is deprecated and will no longer be maintained after March 31, 2026.",
      "We recommend migrating to the QuartzBio R client: https://github.com/quartzbio/quartzbio.edp",
    sep = "\n")
    if (.solveEnv$host != "") {
      link <- paste(sub(".api", "", .solveEnv$host), "/swagger", sep='')
      deprecate_msg <- sprintf("%s\nOr using the QuartzBio REST API: %s", deprecate_msg, link)
    }
    packageStartupMessage(deprecate_msg)
}
