#' World map of a marine spatial metric
#' 
#' @description
#' Plots a marine spatially-explicit metric on a World map. Basemap layers come 
#' from Natural Earth website \url{https://www.naturalearthdata.com/}. The map
#' is plotted in the Robinson projection (`EPSG = 54030`).
#' 
#' @param data an `sf` object. The spatial layer (`POLYGONS`, `POINTS`, etc.) 
#'   containing the variable `metric` for which values are to be mapped. For 
#'   aesthetic purposes, this layer should contains only marine values. 
#'   See `ggmap_freshwater()` for mapping a freshwater (or terrestrial) layer.
#'
#' @param metric a `character` of length 1. The column in `data` for which 
#'   values are to be mapped.
#' 
#' @param title a `character` of length 1. The title of the map (legend title).
#'   Default is `NULL` (no title).
#' 
#' @param palette a `character` of colors used to categorize the values of 
#'   `metric`. Default is `viridisLite::turbo()` with 100 colors.
#' 
#' @return A `ggplot` object.
#' 
#' @export
#' 
#' @examples
#' # Path to example marine layer
#' file_name <- system.file(file.path("extdata", "marine_fish_richness.gpkg"), 
#'                          package = "rphenofish")
#' 
#' # Import example larine layer
#' marine_fish_richness <- sf::st_read(file_name)
#' marine_fish_richness
#'
#' # Map marine fish richness
#' ggmap_marine(data   = marine_fish_richness, 
#'              metric = "richness",
#'              title  = "Number of marine fish species")

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
  
  
  ## Correct MEOW polygons ----
  
  data <- correct_meow(data)
  
  
  ## Project CRS if required ----
  
  if (sf::st_crs(data) != sf::st_crs(ne_bbox)) {
    data <- sf::st_transform(data, crs = sf::st_crs(ne_bbox))  
  }
  
  
  ## Make map ----
  
  gg_map <- ggplot2::ggplot() +
    
    ### Add layers ----
  
    ggplot2::geom_sf(data = ne_bbox, fill = "#cdeafc", col = NA, 
                     linewidth = 0.10) +
    ggplot2::geom_sf(data = ne_graticules, col = "#bae2fb", 
                     linewidth = 0.10) +
    
    ggplot2::geom_sf(data = data, ggplot2::aes(fill = .data[[metric]]),
                     col = "grey30", linewidth = 0.10) +
    
    ggplot2::geom_sf(data = ne_countries, fill = "#c0c0c0", col = "#c9c9c9", 
                     linewidth = 0.10) +
    ggplot2::geom_sf(data = ne_world, fill = NA, col = "grey30", 
                     linewidth = 0.10) +
    
    ggplot2::geom_sf(data = ne_poles, fill = "white", col = "white") +
    ggplot2::geom_sf(data = ne_bbox, fill = NA, col = "#a6a6a6", 
                     linewidth = 0.10) +
    
    
    ### Apply colors palette ----
  
    ggplot2::scale_fill_gradientn(colours = palette, na.value = "dark") +
    
    
    ### Customize theme ----
  
    ggplot2::theme_void() +
    
    ggplot2::theme(legend.position  = "bottom",
                   legend.key.width = ggplot2::unit(1.5, "cm"),
                   legend.title = ggplot2::element_text(face = "bold"))
  
  
  ### Add map title ----
  
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
