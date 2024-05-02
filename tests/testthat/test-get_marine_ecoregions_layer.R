## Test get_marine_ecoregions_layer() ----

### Tests for errors ----

test_that("Test get_marine_ecoregions_layer() for error", {
  
  expect_error(get_marine_ecoregions_layer(path = NULL),
               "Argument 'path' cannot be `NULL`",
               fixed = TRUE)
  
  expect_error(get_marine_ecoregions_layer(path = NA),
               "Argument 'path' cannot contain `NA`",
               fixed = TRUE)
  
  expect_error(get_marine_ecoregions_layer(path = 1),
               "Argument 'path' must be a `character`",
               fixed = TRUE)
  
  expect_error(get_marine_ecoregions_layer(path = c("data", "outputs")),
               "Argument 'path' must be of length 1",
               fixed = TRUE)
  
  expect_error(get_marine_ecoregions_layer(overwrite = NULL),
               "Argument 'overwrite' cannot be `NULL`",
               fixed = TRUE)
  
  expect_error(get_marine_ecoregions_layer(overwrite = NA),
               "Argument 'overwrite' cannot contain `NA`",
               fixed = TRUE)
  
  expect_error(get_marine_ecoregions_layer(overwrite = 2),
               "Argument 'overwrite' must be a `logical`",
               fixed = TRUE)
  
  expect_error(get_marine_ecoregions_layer(overwrite = "2"),
               "Argument 'overwrite' must be a `logical`",
               fixed = TRUE)
  
  expect_error(get_marine_ecoregions_layer(overwrite = c(TRUE, TRUE)),
               "Argument 'overwrite' must be of length 1",
               fixed = TRUE)
  
  expect_error(get_marine_ecoregions_layer(timeout = NULL),
               "Argument 'timeout' cannot be `NULL`",
               fixed = TRUE)
  
  expect_error(get_marine_ecoregions_layer(timeout = NA),
               "Argument 'timeout' cannot contain `NA`",
               fixed = TRUE)
  
  expect_error(get_marine_ecoregions_layer(timeout = c(1, 2)),
               "Argument 'timeout' must be of length 1",
               fixed = TRUE)
  
  expect_error(get_marine_ecoregions_layer(timeout = "2"),
               "Argument 'timeout' must be a `numeric`",
               fixed = TRUE)
  
  expect_error(get_marine_ecoregions_layer(timeout = FALSE),
               "Argument 'timeout' must be a `numeric`",
               fixed = TRUE)
  
  expect_error(get_marine_ecoregions_layer(timeout = 0),
               "Argument 'timeout' must be strictly positive",
               fixed = TRUE)
  
  expect_error(get_marine_ecoregions_layer(timeout = -60),
               "Argument 'timeout' must be strictly positive",
               fixed = TRUE)
})


### Tests for success ----

#### Path exists ----

test_that("Test get_marine_ecoregions_layer() for success", {
  
  skip_on_cran()
  
  create_tempdir()
  dir.create("data")
  
  expect_message(
    get_marine_ecoregions_layer(
      path = "data",
      headers = c("User-Agent" = paste0("Mozilla/5.0 (Windows NT 10.0; Win64; ",
                                        "x64; rv:125.0) Gecko/20100101 ", 
                                        "Firefox/125.0"))),
    "Marine ecoregions layer has been successfully saved in 'data/MEOW/'",
    fixed = TRUE)
  
  expect_false(file.exists(file.path("data", "7gger8mmke_MEOW_FINAL.zip")))
  
  expect_true(dir.exists(file.path("data", "MEOW")))
  expect_true(file.exists(file.path("data", "MEOW", "meow_ecos.shp")))
  
  expect_message(
    get_marine_ecoregions_layer(
      path = "data",
      headers = c("User-Agent" = paste0("Mozilla/5.0 (Windows NT 10.0; Win64; ",
                                        "x64; rv:125.0) Gecko/20100101 ", 
                                        "Firefox/125.0"))),
    "Marine ecoregions layer is already available in 'data/MEOW/'",
    fixed = TRUE)
  
  expect_false(file.exists(file.path("data", "7gger8mmke_MEOW_FINAL.zip")))
  
  expect_true(dir.exists(file.path("data", "MEOW")))
  expect_true(file.exists(file.path("data", "MEOW", "meow_ecos.shp")))
})


#### Path doesn't exist ----

test_that("Test get_marine_ecoregions_layer() for success", {
  
  skip_on_cran()
  
  create_tempdir()
  
  expect_message(
    get_marine_ecoregions_layer(
      path = "data",
      headers = c("User-Agent" = paste0("Mozilla/5.0 (Windows NT 10.0; Win64; ",
                                        "x64; rv:125.0) Gecko/20100101 ", 
                                        "Firefox/125.0"))),
    "Marine ecoregions layer has been successfully saved in 'data/MEOW/'",
    fixed = TRUE)
  
  expect_false(file.exists(file.path("data", "7gger8mmke_MEOW_FINAL.zip")))
  
  expect_true(dir.exists(file.path("data")))
  expect_true(dir.exists(file.path("data", "MEOW")))
  expect_true(file.exists(file.path("data", "MEOW", "meow_ecos.shp")))
  
  
  expect_message(
    get_marine_ecoregions_layer(
      path = "data",
      headers = c("User-Agent" = paste0("Mozilla/5.0 (Windows NT 10.0; Win64; ",
                                        "x64; rv:125.0) Gecko/20100101 ", 
                                        "Firefox/125.0"))),
    "Marine ecoregions layer is already available in 'data/MEOW/'",
    fixed = TRUE)
  
  expect_false(file.exists(file.path("data", "7gger8mmke_MEOW_FINAL.zip")))
  
  expect_true(dir.exists(file.path("data", "MEOW")))
  expect_true(file.exists(file.path("data", "MEOW", "meow_ecos.shp")))
})
