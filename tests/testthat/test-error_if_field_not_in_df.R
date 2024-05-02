## Test error_if_field_not_in_df() ----

### Data for tests ----

test_error_if_field_not_in_df <- function(data, column) {
  error_if_field_not_in_df(data, column)
  invisible(NULL)
}

good_data <- sf::st_read(system.file(file.path("extdata", 
                                               "marine_fish_richness.gpkg"), 
                                     package = "rphenofish"), quiet = TRUE)
bad_col  <- "missing_column"
good_col <- "ECOREGION"


### Tests for errors ----

test_that("Test error_if_field_not_in_df() for error", {
  
  expect_error(error_if_field_not_in_df(good_data, bad_col),
               "The column listed in 'bad_col' is absent from 'good_data'",
               fixed = TRUE)
  
  expect_error(test_error_if_field_not_in_df(good_data, bad_col),
               "The column listed in 'column' is absent from 'data'",
               fixed = TRUE)
  
  expect_error(error_if_field_not_in_df(good_data, c(good_col, good_col)),
               "Argument 'c(good_col, good_col)' must be a `character` of length 1",
               fixed = TRUE)
  
  expect_error(test_error_if_field_not_in_df(good_data, c(good_col, good_col)),
               "Argument 'column' must be a `character` of length 1",
               fixed = TRUE)
})


### Tests for success ----

test_that("Test error_if_field_not_in_df() for success", {
  
  expect_silent(error_if_field_not_in_df(good_data, good_col))
  expect_silent(test_error_if_field_not_in_df(good_data, good_col))
  
  expect_invisible(x <- error_if_field_not_in_df(good_data, good_col))
  expect_invisible(x <- test_error_if_field_not_in_df(good_data, good_col))
})
