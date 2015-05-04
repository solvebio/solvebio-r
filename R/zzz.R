.onLoad <- function(libname = find.package("solvebio"), pkgname = "solvebio") {
    .solveEnv$current$api_host <- Sys.getenv('SOLVEBIO_API_HOST',
                                             unset='https://api.solvebio.com')
    .solveEnv$current$api_key <- Sys.getenv('SOLVEBIO_API_KEY', unset='')
}
