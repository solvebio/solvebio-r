unloadNamespace("solvebio")
library(solvebio)

print_env <- function(e) {
    mget(names(e), e)
}

# the token is stored in p4m-qb-secrets/pfm secrets
edp_envl <- list(
    host = "https://api.solvebio.com",
    token_type = "Bearer",
    # token = "Z9xxxxwUb"
)
edp_env <- as.environment(edp_envl)
print_env(edp_env)

solvebio::Task.retrieve("1922544966688505286", env = edp_env)
solvebio::Task.follow("1922544966688505286", env = edp_env)
solvebio::Task.follow