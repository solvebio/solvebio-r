library(solvebio)
context("Local client environments")

source("./helpers.R", local=T)

test_that("createEnv properly sets defaults", {
              env <- solvebio::createEnv("token")
              expect_equal(env$token, "token")
              expect_equal(env$token_type, "Token")
              expect_equal(env$host, "https://api.solvebio.com")
})

test_that("A default env is created", {
            assert_api_key()
            env <- solvebio::.solveEnv
            expect_equal(env$token, Sys.getenv('SOLVEBIO_API_KEY'))
})
