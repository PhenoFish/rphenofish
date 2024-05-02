## Test error_if_not_character() ----

### Data for tests ----

test_error_if_not_character <- function(data) {
  error_if_not_character(data)
  invisible(NULL)
}


### Tests for errors ----

test_that("Test error_if_not_character() for error", {
  
  expect_error(error_if_not_character(1),
               "Argument '1' must be a `character`",
               fixed = TRUE)
  
  expect_error(error_if_not_character(iris),
               "Argument 'iris' must be a `character`",
               fixed = TRUE)
  
  expect_error(error_if_not_character(NULL),
               "Argument 'NULL' must be a `character`",
               fixed = TRUE)
  
  expect_error(test_error_if_not_character(1),
               "Argument 'data' must be a `character`",
               fixed = TRUE)

  expect_error(test_error_if_not_character(iris),
               "Argument 'data' must be a `character`",
               fixed = TRUE)
  
  expect_error(test_error_if_not_character(NULL),
               "Argument 'data' must be a `character`",
               fixed = TRUE)
})


### Tests for success ----

test_that("Test error_if_not_character() for success", {
  
  expect_silent(error_if_not_character("this is a character"))
  expect_silent(test_error_if_not_character("this is a character"))
  
  expect_invisible(x <- error_if_not_character("this is a character"))
  expect_invisible(x <- test_error_if_not_character("this is a character"))
})
