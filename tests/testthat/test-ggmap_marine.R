## Test ggmap_marine() ----

### Data for tests ----

good_data <- sf::st_read(system.file(file.path("extdata", 
                                               "marine_fish_richness.gpkg"), 
                                     package = "rphenofish"), quiet = TRUE)

good_data_crs <- sf::st_transform(good_data, crs = sf::st_crs(ne_bbox))
bad_data_crs <- good_data_crs

wrong_polygons <- c("Fiji Islands", "Gilbert/Ellis Islands")
good_data_crs <- good_data_crs[!(good_data_crs$"ECOREGION" %in% wrong_polygons), ]


### Tests for errors ----

test_that("Test ggmap_marine() for error", {
  
  expect_error(ggmap_marine(),
               "Argument 'data' is required",
               fixed = TRUE)
  
  expect_error(ggmap_marine(iris),
               "Argument 'data' must be an `sf` object",
               fixed = TRUE)
  
  expect_error(ggmap_marine(good_data),
               "Argument 'metric' is required",
               fixed = TRUE)
  
  expect_error(ggmap_marine(good_data, 1),
               "Argument 'metric' must be a `character`",
               fixed = TRUE)
  
  expect_error(ggmap_marine(good_data, c("ECOREGION", "richness")),
               "Argument 'metric' must be of length 1",
               fixed = TRUE)
  
  expect_error(ggmap_marine(good_data, "missing_col"),
               "The column listed in 'metric' is absent from 'data'",
               fixed = TRUE)
  
  expect_error(ggmap_marine(good_data, "ECOREGION"),
               "The column 'ECOREGION' in 'data' must be `numeric`",
               fixed = TRUE)
  
  expect_error(ggmap_marine(good_data, "richness", title = 1),
               "Argument 'title' must be a `character`",
               fixed = TRUE)
  
  expect_error(ggmap_marine(good_data, "richness", title = NA),
               "Argument 'title' must be a `character`",
               fixed = TRUE)
  
  expect_error(ggmap_marine(good_data, "richness", title = letters),
               "Argument 'title' must be of length 1",
               fixed = TRUE)
  
  expect_error(ggmap_marine(good_data, "richness", title = NULL, palette = 1),
               "Argument 'palette' must be a `character`",
               fixed = TRUE)
  
  expect_error(ggmap_marine(bad_data_crs, "richness"),
               "Spatial layer 'data' must be defined in the WGS 84 system",
               fixed = TRUE)
})


### Tests for success ----

test_that("Test ggmap_marine() for success", {
  
  #### Default ----
  
  expect_silent(x <- ggmap_marine(good_data, "richness"))
   
  expect_true("ggplot" %in% class(x))
  expect_equal(length(x$"layers"), 7L)
  expect_true("element_blank" %in% class(x$"theme"$"legend.title"))
  
  
  #### Change title ----
  
  expect_silent(x <- ggmap_marine(good_data, "richness", 
                                  title = "Species richness"))
  
  expect_true("ggplot" %in% class(x))
  expect_equal(length(x$"layers"), 7L)
  expect_true("element_text" %in% class(x$"theme"$"legend.title"))
  
  
  #### Change palette ----
  
  expect_silent(x <- ggmap_marine(good_data, "richness", 
                                  palette = viridisLite::inferno(100)))
  
  expect_true("ggplot" %in% class(x))
  expect_equal(length(x$"layers"), 7L)
  expect_true("element_blank" %in% class(x$"theme"$"legend.title"))
  
  
  #### No projection required ----
  
  expect_silent(x <- ggmap_marine(good_data_crs, "richness"))
  
  expect_true("ggplot" %in% class(x))
  expect_equal(length(x$"layers"), 7L)
  expect_true("element_blank" %in% class(x$"theme"$"legend.title"))
})
