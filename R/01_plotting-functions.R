## writing functions to map the data 
library(tidyverse)

## source shared functions 
source("R/00_shared-functions.R")

## read in the test data 
load("data-raw/test_phenofish.RData")
data <- test_phenofish

## convert lat lon to numeric 
data$latitude <- as.numeric(as.character(data$latitude))
data$longitude <- as.numeric(as.character(data$longitude))

## add fake classification info
data$genus = c("Bloops")
data$family = c("Bloopidae")
data$order = c("Fisherii")
data$class = c("Swimmyfishii")
data$realm = rep(c("Freshwater", "Marine"), 1000)[1:nrow(data)]


## set all arguments to NULL by default
fishbase_names = NULL 
taxo_level = NULL
taxo_levels = NULL
datasets = NULL
realms = NULL
taxo_scale_observation = NULL
trait_names = NULL
trait_types = NULL
geographic_region = NULL

## set arguments to different things to test
fishbase_names <- unique(data$fishbase_name)[1:10]
taxo_level = "family"
taxo_levels = "Bloopidae"
realms <- c("Freshwater")
geographic_region = "global"


## test the filter by arguments function 
test <- filter_by_arguments(fishbase_names = NULL, 
                    taxo_level = taxo_level,
                    taxo_levels = taxo_levels,
                    datasets = datasets,
                    realms = realms, 
                    taxo_scale_observation = taxo_scale_observation,
                    trait_names = trait_names,
                    trait_types = trait_types,
                    geographic_region = geographic_region, 
                    data = data)

plot_observations <- function(fishbase_names = NULL, 
                              taxo_level = NULL,
                              taxo_levels = NULL,
                              datasets = NULL,
                              realms = NULL, 
                              taxo_scale_observation = NULL,
                              trait_names = NULL,
                              trait_types = NULL,
                              geographic_region = geographic_region, 
                              data = data) {
  
  ## filter Phenofish data by arguments:
  data_sub <- filter_by_arguments(.fishbase_names = fishbase_names, 
                      .taxo_level = taxo_level,
                      .taxo_levels = taxo_levels,
                      .datasets = datasets,
                      .realms = realms, 
                      .taxo_scale_observation = taxo_scale_observation,
                      .trait_names = trait_names,
                      .trait_types = trait_types,
                      .geographic_region = geographic_region, 
                      .data = data)
  
  ## if dataset is null
  if(is.null(data_sub)) {
    print("Cannot plot observations! Error in data filtering")
    return()
  }
  ## or if filtering resulted in dataset with no geo referenced data  
  else if(length(which(is.na(data_sub$latitude))) == nrow(data_sub)) {
    print("Cannot plot observations! No observations in filtered dataset contain geographic coordinates")
    return()
  }
  
  ## otherwise, make the ggplot
  library(ggplot2)
  countries <- map_data("world")
  
  ## remove missing points 
  data_sub <- data_sub[which(!is.na(data_sub$latitude)) & which(!is.na(data_sub$longitude)), ]
  
  map <- ggplot(countries, aes(x=long, y=lat, group = group)) + theme_minimal() + 
    geom_polygon(fill = "grey") + 
    coord_fixed() + 
    geom_point(data = data_sub, aes(y = latitude, x = longitude), size = 1, 
               inherit.aes = F) + 
    labs(x =  "Latitude", y = "Longitude") +
    theme(legend.title = element_text(size = 9))
  
 return(map) 
}


## test it 
plot_observations(fishbase_names = fishbase_names, 
                            taxo_level = NULL,
                            taxo_levels = NULL,
                            datasets = datasets,
                            realms = realms, 
                            taxo_scale_observation = taxo_scale_observation,
                            trait_names = trait_names,
                            trait_types = trait_types,
                            geographic_region = geographic_region, 
                            data = data)

plot_observations(fishbase_names = fishbase_names, 
                  taxo_level = NULL,
                  taxo_levels = NULL,
                  datasets = c("fishmorph"),
                  realms = realms, 
                  taxo_scale_observation = taxo_scale_observation,
                  trait_names = trait_names,
                  trait_types = trait_types,
                  geographic_region = geographic_region, 
                  data = data)







plot_phylo <- function() {}

plot_diagnostics <- function() {}

plot_by_ecoregion <- function() {}



## add "save" as a parameter


