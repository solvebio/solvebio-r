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
