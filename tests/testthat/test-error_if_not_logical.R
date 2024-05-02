## Test error_if_not_logical() ----

### Data for tests ----

test_error_if_not_logical <- function(data) {
  error_if_not_logical(data)
  invisible(NULL)
}


### Tests for errors ----

test_that("Test error_if_not_logical() for error", {
  
  expect_error(error_if_not_logical("1"),
               "Argument '\"1\"' must be a `logical`",
               fixed = TRUE)
  
  expect_error(error_if_not_logical(2),
               "Argument '2' must be a `logical`",
               fixed = TRUE)
  
  expect_error(error_if_not_logical(iris),
               "Argument 'iris' must be a `logical`",
               fixed = TRUE)
  
  expect_error(error_if_not_logical(NULL),
               "Argument 'NULL' must be a `logical`",
               fixed = TRUE)
  
  expect_error(test_error_if_not_logical("1"),
               "Argument 'data' must be a `logical`",
               fixed = TRUE)
  
  expect_error(test_error_if_not_logical(2),
               "Argument 'data' must be a `logical`",
               fixed = TRUE)
  
  expect_error(test_error_if_not_logical(iris),
               "Argument 'data' must be a `logical`",
               fixed = TRUE)
  
  expect_error(test_error_if_not_logical(NULL),
               "Argument 'data' must be a `logical`",
               fixed = TRUE)
})


### Tests for success ----

test_that("Test error_if_not_logical() for success", {
  
  expect_silent(error_if_not_logical(TRUE))
  expect_silent(test_error_if_not_logical(FALSE))
  
  expect_invisible(x <- error_if_not_logical(FALSE))
  expect_invisible(x <- test_error_if_not_logical(TRUE))
})
