library(solvebio)
context("Test Dataset.query")

source("./helpers.R", local=T)

# NOTE: good_env only works if assert_api_key()
env <- solvebio::createEnv(solvebio:::.solveEnv$token)

TEST_DATASET_FULL_PATH_2 <- 'solvebio:public:/ClinVar/5.1.0-20200720/Variants-GRCH38'

test_that("Query the dataset and check that titles are colnames in df", {
              assert_api_key()

              user <- User.retrieve(env=env)
              expect_equal(user$email, "test@solvebio.com")

              ds <- Dataset.get_by_full_path(TEST_DATASET_FULL_PATH_2)
              query <- Dataset.query(ds$id, fields=list("variant", "_id", "disease", "allele"), use_field_titles=TRUE)
              expect_true('_id' %in% colnames(query))

})