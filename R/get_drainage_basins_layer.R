#' Download World drainage basins spatial layer
#'
#' @description
#' Downloads the spatial layer of drainage basins of the World available on the 
#' Figshare website 
#' (<https://figshare.com/collections/A_global_database_on_freshwater_fish_species_occurrences_in_drainage_basins/3739145>).
#'
#' @param path a `character` of length 1. The path to the folder to save the 
#'   spatial layer. Note that a subdirectory `basins/` will be created.
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
#' Tedesco P _et al._ (2017) A global database on freshwater fish species 
#' occurrence in drainage basins. Scientific Data, 4, 170141.
#' <https://doi.org/10.1038/sdata.2017.141>.
#'
#' @examples
#' \dontrun{
#' ## Download drainage basins spatial layer ----
#' get_drainage_basins_layer(path = "data")
#' }

get_drainage_basins_layer <- function(path = ".", overwrite = FALSE, 
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
  
  if (!("Basin042017_3119.shp" %in% list.files(file.path(path, "basins")))) {
    
    
    ## Change timeout for slow connection ----
    
    user_opts <- options()
    on.exit(options(user_opts))
    options(timeout = max(timeout, getOption("timeout")))
    
    
    ## URL parameters ----
    
    baseurl <- "https://figshare.com/ndownloader/files/8964583/"
    zipname <- "datatoFigshare.zip"
    
    
    ## Download ZIP file ----
    
    utils::download.file(url      = paste0(baseurl, zipname),
                         destfile = file.path(path, zipname), 
                         mode     = "wb",
                         quiet    = TRUE, ...)
    
    
    ## Extract files in ZIP ----
    
    utils::unzip(zipfile = file.path(path, zipname), 
                 exdir   = file.path(path, "basins"))
    
    
    ## Delete ZIP file ----
    
    invisible(file.remove(file.path(path, zipname)))
    
    
    ## Delete optional files ----
    
    fls_to_del <- list.files(file.path(path, "basins"), pattern = "\\.csv$")
    
    for (fls in fls_to_del) {
      invisible(file.remove(file.path(path, "basins", fls)))
    }
    
    message("Drainage basins spatial layer has been successfully saved in '", 
            file.path(path, "basins"), "/'")
    
  } else {
    
    message("Drainage basins spatial layer is already available in '", 
            file.path(path, "basins"), "/'")
  }
  
  invisible(NULL)
}
