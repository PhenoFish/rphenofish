## Test error_if_not_numeric() ----

### Data for tests ----

test_error_if_not_numeric <- function(data) {
  error_if_not_numeric(data)
  invisible(NULL)
}


### Tests for errors ----

test_that("Test error_if_not_numeric() for error", {
  
  expect_error(error_if_not_numeric("1"),
               "Argument '\"1\"' must be a `numeric`",
               fixed = TRUE)
  
  expect_error(error_if_not_numeric(iris),
               "Argument 'iris' must be a `numeric`",
               fixed = TRUE)
  
  expect_error(error_if_not_numeric(NULL),
               "Argument 'NULL' must be a `numeric`",
               fixed = TRUE)
  
  expect_error(test_error_if_not_numeric("1"),
               "Argument 'data' must be a `numeric`",
               fixed = TRUE)
  
  expect_error(test_error_if_not_numeric(iris),
               "Argument 'data' must be a `numeric`",
               fixed = TRUE)
  
  expect_error(test_error_if_not_numeric(NULL),
               "Argument 'data' must be a `numeric`",
               fixed = TRUE)
})


### Tests for success ----

test_that("Test error_if_not_numeric() for success", {
  
  expect_silent(error_if_not_numeric(1))
  expect_silent(test_error_if_not_numeric(1))
  
  expect_invisible(x <- error_if_not_numeric(1:10))
  expect_invisible(x <- test_error_if_not_numeric(1:10))
})
