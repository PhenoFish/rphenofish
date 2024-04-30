#' Plot a spatial metric at the marine ecoregion level
#' 
#' @description
#' Represents a spatially-explicit metric on a World map at the marine 
#' ecoregions scale.
#' 
#' @param data an `sf` object with the values of `metric` for each MEOW polygon
#'   (in rows).
#'
#' @param metric a `character` of length 1. The column in `data` to map.
#' 
#' @param title a `character` of length 1. The title of the map (legend title).
#'   Default is `NULL` (no title).
#' 
#' @param palette a `character` of colors used to categorize the values of 
#'   `metric`. Default is `viridisLite::turbo()`.
#' 
#' @return A `ggplot` object.
#' 
#' @export
#' 
#' @examples
#' ## Add an example

ggmap_marine <- function(data, metric, title = NULL, 
                         palette = viridisLite::turbo(n = 100)) {
  
  
  ## Check args ----
  
  error_if_missing(data)
  error_if_not_sf(data)
  
  error_if_missing(metric)
  error_if_not_character(metric)
  error_if_not_length_of(metric, 1)
  
  error_if_field_not_in_df(data, metric)
  
  if (!is.null(title)) {
    error_if_not_character(title)
    error_if_not_length_of(title, 1)
  }
  
  error_if_not_character(palette)
  
  
  ## Correct and convert data ----
  
  data <- correct_meow(data)
  data <- sf::st_transform(data, crs = sf::st_crs(ne_bbox))
  
  
  ## Make map ----
  
  gg_map <- ggplot2::ggplot() +
    
    ggplot2::geom_sf(data = ne_bbox, fill = "#cdeafc", col = NA, 
                     linewidth = 0.10) +
    ggplot2::geom_sf(data = ne_graticules, col = "#bae2fb", 
                     linewidth = 0.10) +
    
    ggplot2::geom_sf(data = data, ggplot2::aes(fill = .data[[metric]]),
                     col = "grey30") +
    
    ggplot2::scale_fill_gradientn(colours = palette, na.value = "dark") +
    
    ggplot2::geom_sf(data = ne_countries, fill = "#c0c0c0", col = "#c9c9c9", 
                     linewidth = 0.10) +
    ggplot2::geom_sf(data = ne_world, fill = NA, col = "grey30", 
                     linewidth = 0.10) +
    
    ggplot2::geom_sf(data = ne_poles, fill = "white", col = "white") +
    ggplot2::geom_sf(data = ne_bbox, fill = NA, col = "#a6a6a6", 
                     linewidth = 0.10) +
    
    ggplot2::theme_void() +
    
    ggplot2::theme(legend.position  = "bottom",
                   legend.key.width = ggplot2::unit(1.5, "cm"),
                   legend.title = ggplot2::element_text(face = "bold"))
  
  if (!is.null(title)) {
    
    gg_map <- gg_map +
      
      ggplot2::labs(fill = title) + 
      
      ggplot2::guides(fill = ggplot2::guide_colorbar(title.position = "top", 
                                                     title.hjust = 0.5))
  } else {
    
    gg_map <- gg_map +
      
      ggplot2::theme(legend.title = ggplot2::element_blank())
  }
    
  gg_map
}
