## Test correct_meow_polygons() ----

### Data for tests ----

good_data <- sf::st_read(system.file(file.path("extdata", 
                                               "marine_fish_richness.gpkg"), 
                                     package = "rphenofish"), quiet = TRUE)

wrong_polygons <- c("Fiji Islands", "Gilbert/Ellis Islands")
ready_data <- good_data[!(good_data$"ECOREGION" %in% wrong_polygons), ]

bad_data_crs <- sf::st_transform(good_data, crs = sf::st_crs(ne_bbox))


### Tests for errors ----

test_that("Test correct_meow_polygons() for error", {
  
  expect_error(correct_meow_polygons(iris),
               "Argument 'data' must be an `sf` object",
               fixed = TRUE)
  
  expect_error(correct_meow_polygons(good_data[ , -2]),
               "The column listed in '\"ECOREGION\"' is absent from 'data'",
               fixed = TRUE)
  
  expect_error(correct_meow_polygons(bad_data_crs),
               "Spatial layer 'data' must be defined in the WGS 84 system",
               fixed = TRUE)
})


### Tests for success ----

test_that("Test correct_meow_polygons() for success", {
  
  expect_invisible(x <- correct_meow_polygons(good_data))
  
  expect_true("sf" %in% class(x))
  expect_true(nrow(x) == 235L)
  expect_true(ncol(x) == ncol(good_data))
  expect_equal(sum(colnames(x) == colnames(good_data)), 4L)
  
  expect_invisible(x <- correct_meow_polygons(ready_data))
  
  expect_true("sf" %in% class(x))
  expect_true(nrow(x) == 230L)
  expect_true(ncol(x) == ncol(good_data))
  expect_equal(sum(colnames(x) == colnames(ready_data)), 4L)
})
