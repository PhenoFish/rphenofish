## Test error_if_missing() ----

### Data for tests ----

test_error_if_missing <- function(data) {
  error_if_missing(data)
  invisible(NULL)
}


### Tests for errors ----

test_that("Test error_if_missing() for error", {
  
  expect_error(error_if_missing(),
               "Argument '' is required",
               fixed = TRUE)
  
  expect_error(test_error_if_missing(),
               "Argument 'data' is required",
               fixed = TRUE)
})


### Tests for success ----

test_that("Test error_if_missing() for success", {
  
  expect_silent(error_if_missing(iris))
  expect_silent(test_error_if_missing(iris))

  expect_invisible(x <- error_if_missing(iris))
  expect_invisible(x <- test_error_if_missing(iris))
})
