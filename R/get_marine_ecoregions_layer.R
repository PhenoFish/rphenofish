#' Download marine ecoregions of the World (MEOW) spatial layer
#'
#' @description
#' Downloads the spatial layer of marine ecoregions of the World (MEOW) 
#' available on the WWF website 
#' (<https://www.worldwildlife.org/publications/marine-ecoregions-of-the-world-a-bioregionalization-of-coastal-and-shelf-areas>).
#'
#' @param path a `character` of length 1. The path to the folder to save the 
#'   spatial layer. Note that a subdirectory `MEOW/` will be created.
#'   
#' @param overwrite a `logical`. If `TRUE` it will override the existing files. 
#'   Default is `FALSE`.
#'   
#' @param timeout an `integer`. The timeout for downloading files. 
#'   Default is `60`. This number can be increased for slow Internet connection.
#'   
#' @param ... other arguments to pass to `download.file()` (e.g. `headers`).
#'   
#' @return No return value. Files will be written in `path`.
#' 
#' @export
#' 
#' @references
#' Spalding MD _et al._ (2007) Marine Ecoregions of the World: A 
#' Bioregionalization of Coastal and Shelf Areas. BioScience, 57(7), 573-583.
#' <https://doi.org/10.1641/B570707>.
#'
#' @examples
#' \dontrun{
#' ## Download marine ecoregions spatial layer ----
#' get_marine_ecoregions_layer(path = "data")
#' }

get_marine_ecoregions_layer <- function(path = ".", overwrite = FALSE, 
                                        timeout = 60, ...) {
  
  
  ## Check path ----
  
  error_if_null(path)
  error_if_na(path)
  error_if_not_character(path)
  error_if_not_length_of(path, 1)

  
  ## Check other args ----
  
  error_if_null(overwrite)
  error_if_na(overwrite)
  error_if_not_logical(overwrite)
  error_if_not_length_of(overwrite, 1)
  
  error_if_null(timeout)
  error_if_na(timeout)
  error_if_not_numeric(timeout)
  error_if_not_length_of(timeout, 1)
  error_if_not_strictly_positive(timeout)
  
  
  ## Create path if required ----
  
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }
  
  
  ## Check if layer is already locally available ----
  
  if (!("meow_ecos.shp" %in% list.files(file.path(path, "MEOW")))) {
    
    
    ## Change timeout for slow connection ----
    
    user_opts <- options()
    on.exit(options(user_opts))
    options(timeout = max(timeout, getOption("timeout")))
    
    
    ## URL parameters ----
    
    baseurl <- 
      "https://files.worldwildlife.org/wwfcmsprod/files/Publication/file/"
    zipname <- "7gger8mmke_MEOW_FINAL.zip"
    
    
    ## Download ZIP file ----
    
    utils::download.file(url      = paste0(baseurl, zipname),
                         destfile = file.path(path, zipname), 
                         mode     = "wb",
                         quiet    = TRUE, ...)
    
    
    ## Extract files in ZIP ----
    
    utils::unzip(zipfile = file.path(path, zipname), exdir = path)
    
    
    ## Delete ZIP file ----
    
    invisible(file.remove(file.path(path, zipname)))
    
    message("Marine ecoregions layer has been successfully saved in '", 
            file.path(path, "MEOW"), "/'")
    
  } else {
    
    message("Marine ecoregions layer is already available in '", 
            file.path(path, "MEOW"), "/'")
  }
  
  invisible(NULL)
}
