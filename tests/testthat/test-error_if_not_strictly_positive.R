## Test error_if_not_strictly_positive() ----

### Data for tests ----

test_error_if_not_strictly_positive <- function(data) {
  error_if_not_strictly_positive(data)
  invisible(NULL)
}


### Tests for errors ----

test_that("Test error_if_not_strictly_positive() for error", {
  
  expect_error(error_if_not_strictly_positive(-10),
               "Argument '-10' must be strictly positive",
               fixed = TRUE)
  
  expect_error(test_error_if_not_strictly_positive("1"),
               "Argument 'x' must be a `numeric`",
               fixed = TRUE)
  
  expect_error(test_error_if_not_strictly_positive(-10),
               "Argument 'data' must be strictly positive",
               fixed = TRUE)
  
  expect_error(test_error_if_not_strictly_positive(0),
               "Argument 'data' must be strictly positive",
               fixed = TRUE)
  
  expect_error(test_error_if_not_strictly_positive(c(-10, 10, 100)),
               "Argument 'data' must be strictly positive",
               fixed = TRUE)
  
  expect_error(test_error_if_not_strictly_positive(c(-10, 10, NA)),
               "Argument 'x' cannot contain `NA`",
               fixed = TRUE)
})


### Tests for success ----

test_that("Test error_if_not_strictly_positive() for success", {
  
  expect_silent(error_if_not_strictly_positive(1))
  expect_silent(error_if_not_strictly_positive(c(10, 0.001)))
  
  expect_silent(test_error_if_not_strictly_positive(1))
  expect_silent(test_error_if_not_strictly_positive(c(10, 0.001)))

  expect_invisible(x <- error_if_not_strictly_positive(12))
  expect_invisible(x <- test_error_if_not_strictly_positive(1))
})
