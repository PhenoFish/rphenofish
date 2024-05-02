## Test ggmap_terrestrial() ----

### Data for tests ----

good_data <- sf::st_read(system.file(file.path("extdata", 
                                               "freshwater_fish_richness.gpkg"), 
                                     package = "rphenofish"), quiet = TRUE)

good_data_crs <- sf::st_transform(good_data, crs = sf::st_crs(ne_bbox))


### Tests for errors ----

test_that("Test ggmap_terrestrial() for error", {
  
  expect_error(ggmap_terrestrial(),
               "Argument 'data' is required",
               fixed = TRUE)
  
  expect_error(ggmap_terrestrial(iris),
               "Argument 'data' must be an `sf` object",
               fixed = TRUE)
  
  expect_error(ggmap_terrestrial(good_data),
               "Argument 'metric' is required",
               fixed = TRUE)
  
  expect_error(ggmap_terrestrial(good_data, 1),
               "Argument 'metric' must be a `character`",
               fixed = TRUE)
  
  expect_error(ggmap_terrestrial(good_data, c("BasinName", "richness")),
               "Argument 'metric' must be of length 1",
               fixed = TRUE)
  
  expect_error(ggmap_terrestrial(good_data, "missing_col"),
               "The column listed in 'metric' is absent from 'data'",
               fixed = TRUE)
  
  expect_error(ggmap_terrestrial(good_data, "BasinName"),
               "The column 'BasinName' in 'data' must be `numeric`",
               fixed = TRUE)
  
  expect_error(ggmap_terrestrial(good_data, "richness", title = 1),
               "Argument 'title' must be a `character`",
               fixed = TRUE)
  
  expect_error(ggmap_terrestrial(good_data, "richness", title = NA),
               "Argument 'title' must be a `character`",
               fixed = TRUE)
  
  expect_error(ggmap_terrestrial(good_data, "richness", title = letters),
               "Argument 'title' must be of length 1",
               fixed = TRUE)
  
  expect_error(ggmap_terrestrial(good_data, "richness", title = NULL, palette = 1),
               "Argument 'palette' must be a `character`",
               fixed = TRUE)
})


### Tests for success ----

test_that("Test ggmap_terrestrial() for success", {
  
  #### Default ----
  
  expect_silent(x <- ggmap_terrestrial(good_data, "richness"))
  
  expect_true("ggplot" %in% class(x))
  expect_equal(length(x$"layers"), 7L)
  expect_true("element_blank" %in% class(x$"theme"$"legend.title"))
  
  
  #### Change title ----
  
  expect_silent(x <- ggmap_terrestrial(good_data, "richness", 
                                       title = "Species richness"))
  
  expect_true("ggplot" %in% class(x))
  expect_equal(length(x$"layers"), 7L)
  expect_true("element_text" %in% class(x$"theme"$"legend.title"))
  
  
  #### Change palette ----
  
  expect_silent(x <- ggmap_terrestrial(good_data, "richness", 
                                       palette = viridisLite::inferno(100)))
  
  expect_true("ggplot" %in% class(x))
  expect_equal(length(x$"layers"), 7L)
  expect_true("element_blank" %in% class(x$"theme"$"legend.title"))
  
  
  #### No projection required ----
  
  expect_silent(x <- ggmap_terrestrial(good_data_crs, "richness"))
  
  expect_true("ggplot" %in% class(x))
  expect_equal(length(x$"layers"), 7L)
  expect_true("element_blank" %in% class(x$"theme"$"legend.title"))
})
