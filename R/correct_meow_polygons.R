#' Correct MEOW polygon geometries
#'
#' @description
#' Some MEOW polygons cannot be projected without rendering artifact. This 
#' function slightly changes longitude of polygons at the edge of World.
#'
#' @param data an `sf MULTIPOLYGON` object. The MEOW spatial layer.
#'
#' @return A `sf MULTIPOLYGON`.
#' 
#' @noRd

correct_meow_polygons <- function(data) {
  
  ## Check args ----
  
  error_if_not_sf(data)
  error_if_field_not_in_df(data, "ECOREGION")
   
  
  ## Correct polygons ----
  
  wrong_polygons <- c("Fiji Islands", "Gilbert/Ellis Islands")
  
  pos <- which(data$"ECOREGION" %in% wrong_polygons)
  
  if (length(pos) > 0) {
    
    ## Check CRS ----
    
    if (sf::st_crs(data)$"input" != "WGS 84") {
      stop("Spatial layer 'data' must be defined in the WGS 84 system", 
           call. = FALSE)
    }
    

    new_lines <- data.frame()
    
    for (i in 1:length(pos)) {
      
      attrs  <- sf::st_drop_geometry(data[pos[i], ])
      coords <- sf::st_coordinates(data[pos[i], ])
      
      n <- length(unique(coords[ , 4]))
      
      for (j in 1:n) {
        
        polygon <- coords[which(coords[ , 4] == j), ]
        
        polygon[ , 1] <- as.numeric(gsub("180.0000", "179.9999", polygon[ , 1]))
        
        polygon <- sf::st_polygon(list(polygon[ , 1:2]))
        polygon <- sf::st_sfc(polygon, crs = sf::st_crs(4326))
        new_line <- attrs
        sf::st_geometry(new_line) <- polygon
        
        new_lines <- rbind(new_lines, new_line)
      }
    }
    
    data <- data[-pos, ]
    sf::st_geometry(new_lines) <- "geom"
    
    data <- rbind(data, new_lines)
  }
  
  data
}
