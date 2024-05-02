## Test error_if_not_sf() ----

### Data for tests ----

test_error_if_not_sf <- function(data) {
  error_if_not_sf(data)
  invisible(NULL)
}

bad_data  <- iris
good_data <- sf::st_read(system.file(file.path("extdata", 
                                               "marine_fish_richness.gpkg"), 
                                     package = "rphenofish"), quiet = TRUE)


### Tests for errors ----

test_that("Test error_if_not_sf() for error", {
  
  expect_error(error_if_not_sf(bad_data),
               "Argument 'bad_data' must be an `sf` object",
               fixed = TRUE)
  
  expect_error(test_error_if_not_sf(bad_data),
               "Argument 'data' must be an `sf` object",
               fixed = TRUE)
  
  expect_error(error_if_not_sf(good_data[0, ]),
               "Argument 'good_data[0, ]' must have at least one row",
               fixed = TRUE)
  
  expect_error(test_error_if_not_sf(good_data[0, ]),
               "Argument 'data' must have at least one row",
               fixed = TRUE)
})


### Tests for success ----

test_that("Test error_if_not_sf() for success", {
  
  expect_silent(error_if_not_sf(good_data))
  expect_silent(test_error_if_not_sf(good_data))
  
  expect_invisible(x <- error_if_not_sf(good_data))
  expect_invisible(x <- test_error_if_not_sf(good_data))
})
