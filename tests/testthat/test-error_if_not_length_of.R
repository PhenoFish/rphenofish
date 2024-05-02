## Test error_if_not_length_of() ----

### Data for tests ----

test_error_if_not_length_of <- function(data, n) {
  error_if_not_length_of(data, n)
  invisible(NULL)
}



### Tests for errors ----

test_that("Test error_if_not_length_of() for error", {
  
  expect_error(error_if_not_length_of("length_1", n = "length_1"),
               "Argument 'n' must be a `numeric`",
               fixed = TRUE)
  
  expect_error(error_if_not_length_of("length_1", n = NULL),
               "Argument 'n' must be a `numeric`",
               fixed = TRUE)
  
  expect_error(error_if_not_length_of("length_1", n = NA),
               "Argument 'n' must be a `numeric`",
               fixed = TRUE)
  
  expect_error(error_if_not_length_of("length_1", n = 1:2),
               "Argument 'n' must be a `numeric` of length 1",
               fixed = TRUE)
  
  expect_error(error_if_not_length_of("length_1", n = 2),
               "Argument '\"length_1\"' must be of length 2",
               fixed = TRUE)
  
  expect_error(error_if_not_length_of(rep("length_1", 2), n = 1),
               "Argument 'rep(\"length_1\", 2)' must be of length 1",
               fixed = TRUE)
  
  expect_error(test_error_if_not_length_of("length_1", n = "length_1"),
               "Argument 'n' must be a `numeric`",
               fixed = TRUE)
  
  expect_error(test_error_if_not_length_of("length_1", n = NULL),
               "Argument 'n' must be a `numeric`",
               fixed = TRUE)
  
  expect_error(test_error_if_not_length_of("length_1", n = NA),
               "Argument 'n' must be a `numeric`",
               fixed = TRUE)
  
  expect_error(test_error_if_not_length_of("length_1", n = 1:2),
               "Argument 'n' must be a `numeric` of length 1",
               fixed = TRUE)
  
  expect_error(test_error_if_not_length_of("length_1", n = 2),
               "Argument 'data' must be of length 2",
               fixed = TRUE)
  
  expect_error(test_error_if_not_length_of(rep("length_1", 2), n = 1),
               "Argument 'data' must be of length 1",
               fixed = TRUE)
})


### Tests for success ----

test_that("Test error_if_not_length_of() for success", {
  
  expect_silent(error_if_not_length_of("length_1", 1))
  expect_silent(test_error_if_not_length_of("length_1", 1))
  
  expect_invisible(x <- error_if_not_length_of("length_1", 1))
  expect_invisible(x <- test_error_if_not_length_of("length_1", 1))
  
  expect_silent(error_if_not_length_of(rep("length_1", 2), 2))
  expect_silent(test_error_if_not_length_of(rep("length_1", 2), 2))
  
  expect_invisible(x <- error_if_not_length_of(rep("length_1", 2), 2))
  expect_invisible(x <- test_error_if_not_length_of(rep("length_1", 2), 2))
})
