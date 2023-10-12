## writing functions to map the data 
library(tidyverse)

## read in the test data 
load("data-raw/test_phenofish.RData")

data <- test_phenofish

plot_observations <- function(fishbase_name = NULL, 
                              taxo_level = NULL,
                              dataset = NULL,
                              realm = NULL, 
                              taxo_scale_observation = NULL,
                              trait_name = NULL,
                              trait_type = NULL,
                              global = NULL) {
  
  ## filter the full Phenofish dataset according to arguments passed to function
  
 
  
 return() 
}

plot_phylo <- function() {}

plot_diagnostics <- function() {}

plot_by_ecoregion <- function() {}





