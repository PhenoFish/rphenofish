## basic functions that will be shared and called by other functions

## function to fitler by common arguments and throw errors if filtering parameters are wrongly specified 
filter_by_arguments <- function(.fishbase_names = NULL, 
                                .taxo_level = NULL,
                                .taxo_levels = NULL,
                                .datasets = NULL,
                                .realms = NULL, 
                                .taxo_scale_observation = NULL,
                                .trait_names = NULL,
                                .trait_types = NULL,
                                .geographic_region = NULL, 
                                .data = data) {
  
  ## filter the full Phenofish dataset according to arguments passed to function
  ## if taxonomic level AND species names are specified, throw error:
  if(!is.null(.fishbase_names) & !is.null(.taxo_level)) {
    print("Error: either a taxonomic level OR a vector of species names can be specified, not both.")
    return()
  }
  ## if trait names AND trait types are specified, throw error:
  if(!is.null(.trait_names) & !is.null(.trait_types)) {
    print("Error: either a vector of trait names OR a vector of trait types can be specified, not both.")
    return()
  }
  
  data_sub = data
  
  ## filter to species list
  if(!is.null(.fishbase_names)) {
    data_sub <- data[which(data$fishbase_names %in% .fishbase_names),]
  }
  ## filter by taxonomic levels requested
  else if(!is.null(.taxo_level)) {
    if(taxo_level == "genus") {
      if(length(which(!data_sub$genus %in% .taxo_levels)) == nrow(data_sub)) {
        print("Error: no requested genus is in dataset")
        return()
      }
      else {
        data_sub <- data_sub[which(data_sub$genus %in% .taxo_levels),]
      }
    }
    else if(.taxo_level == "family") {
      if(length(which(!data_sub$family %in% taxo_levels)) == nrow(data_sub)) {
        print("Error: no requested family is in dataset")
        return()
      }
      else {
        data_sub <- data_sub[which(data_sub$family %in% .taxo_levels),]
      }
    }
    else if(.taxo_level == "order") {
      if(length(which(!data_sub$order %in% .taxo_levels)) == nrow(data_sub)) {
        print("Error: no requested order is in dataset")
        return()
      }
      else {
        data_sub <- data_sub[which(data_sub$order %in% .taxo_levels),]
      }
    }
    else if(length(which(!data_sub$class %in% .taxo_levels)) == nrow(data_sub)) {
      if(!data_sub$class %in% .taxo_levels) {
        print("Error: no requested class is in dataset")
        return()
      }
      else {
        data_sub <- data_sub[which(data_sub$class %in% .taxo_levels),]
      }
    }
    
    ## filter by datasets
    if(!is.null(.datasets)) {
      data_sub <- data_sub[which(data_sub$dataset %in% .datasets),]
    }
    ## filter by realms
    if(!is.null(.realms)) {
      data_sub <- data_sub[which(data_sub$realm %in% .realms),]
    }
    ## filter by the taxonomic scale of observation
    if(!is.null(.taxo_scale_observation)) {
      data_sub <- data_sub[which(data_sub$taxo_scale_observation %in% .taxo_scale_observation),]
    }
    ## filter to traits of interest
    if(!is.null(.trait_names)) {
      data_sub <- data_sub[which(data_sub$trait_name %in% .trait_names),]
    }
    ## filter to traits of interest
    if(!is.null(.trait_types)) {
      data_sub <- data_sub[which(data_sub$trait_types %in% .trait_types),]
    }
    
    ## filter to geographic region 
    if(.geographic_region != "global") {
      ## make sure specified coordinates make sense
      ## check lat min < lat max, within -90 to 90
      ## check lon min < lon max, within -180 to 180
    }
    
  }
  return(data_sub)
}
