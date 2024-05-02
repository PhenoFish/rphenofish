## Test error_if_null() ----

### Data for tests ----

test_error_if_null <- function(data) {
  error_if_null(data)
  invisible(NULL)
}


### Tests for errors ----

test_that("Test error_if_null() for error", {
  
  expect_error(error_if_null(NULL),
               "Argument 'NULL' cannot be `NULL`",
               fixed = TRUE)
  
  expect_error(test_error_if_null(NULL),
               "Argument 'data' cannot be `NULL`",
               fixed = TRUE)
})


### Tests for success ----

test_that("Test error_if_null() for success", {
  
  expect_silent(error_if_null(1))
  expect_silent(test_error_if_null(1))
  expect_silent(test_error_if_null(1:10))
  expect_silent(test_error_if_null(iris))
  expect_silent(test_error_if_null("1"))
  expect_silent(test_error_if_null(TRUE))
  
  expect_invisible(x <- error_if_null("iris"))
  expect_invisible(x <- test_error_if_null("iris"))
})
