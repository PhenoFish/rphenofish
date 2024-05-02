## Test error_if_na() ----

### Data for tests ----

test_error_if_na <- function(data) {
  error_if_na(data)
  invisible(NULL)
}


### Tests for errors ----

test_that("Test error_if_na() for error", {
  
  expect_error(error_if_na(NA),
               "Argument 'NA' cannot contain `NA`",
               fixed = TRUE)
  
  expect_error(error_if_na(c(1, NA, 1)),
               "Argument 'c(1, NA, 1)' cannot contain `NA`",
               fixed = TRUE)
  
  expect_error(test_error_if_na(NA),
               "Argument 'data' cannot contain `NA`",
               fixed = TRUE)
  
  expect_error(test_error_if_na(c(1, NA, 1)),
               "Argument 'data' cannot contain `NA`",
               fixed = TRUE)
  
  expect_error(test_error_if_na(c("AAA", NA, "AA")),
               "Argument 'data' cannot contain `NA`",
               fixed = TRUE)
})


### Tests for success ----

test_that("Test error_if_na() for success", {
  
  expect_silent(error_if_na(1))
  expect_silent(test_error_if_na(1))
  expect_silent(test_error_if_na(1:10))
  expect_silent(test_error_if_na(iris))
  expect_silent(test_error_if_na("1"))
  expect_silent(test_error_if_na(TRUE))
  
  expect_invisible(x <- error_if_na("iris"))
  expect_invisible(x <- test_error_if_na("iris"))
})
