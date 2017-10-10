.onLoad <- function(libname = find.package("solvebio"), pkgname = "solvebio") {
    .solveEnv$host <- Sys.getenv('SOLVEBIO_API_HOST',
                                 unset='https://api.solvebio.com')
    .solveEnv$token <- Sys.getenv('SOLVEBIO_API_KEY', unset='')
    .solveEnv$token_type <- 'Token'
}
