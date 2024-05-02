#' Return an error if the argument is missing
#' 
#' @noRd

error_if_missing <- function(x) {
  
  if (missing(x)) {
    stop("Argument '", deparse(substitute(x)), "' is required", call. = FALSE)
  }
  
  invisible(NULL)
}




#' Return an error if the argument is not an sf object
#' 
#' @noRd

error_if_not_sf <- function(x) {
  
  if (!inherits(x, "sf")) {
    stop("Argument '", deparse(substitute(x)), "' must be an `sf` object", 
         call. = FALSE)
  }
  
  if (nrow(x) == 0) {
    stop("Argument '", deparse(substitute(x)), "' must have at least one row", 
         call. = FALSE)
  }
  
  invisible(NULL)
}




#' Return an error if a column is absent from a data.frame
#' 
#' @noRd

error_if_field_not_in_df <- function(data, field) {
  
  if (length(field) != 1) {
    stop("Argument '", deparse(substitute(field)), "' must be a `character` of ", 
         "length 1", call. = FALSE)
  }
  
  if (!(field %in% colnames(data))) {
    stop("The column listed in '", deparse(substitute(field)), "' is absent ", 
         "from '", deparse(substitute(data)), "'", call. = FALSE)
  }
  
  invisible(NULL)
}




#' Return an error if the argument is not a character
#' 
#' @noRd

error_if_not_character <- function(x) {
 
  if (!is.character(x)) {
    stop("Argument '", deparse(substitute(x)), "' must be a `character`", 
         call. = FALSE)
  }
  
  invisible(NULL) 
}



#' Return an error if the argument is not of length n
#' 
#' @noRd

error_if_not_length_of <- function(x, n = 1) {

  if (!is.numeric(n)) {
    stop("Argument 'n' must be a `numeric`", call. = FALSE)
  }
  
  if (length(n) != 1) {
    stop("Argument 'n' must be a `numeric` of length 1", call. = FALSE)
  }
  
  if (length(x) != n) {
    stop("Argument '", deparse(substitute(x)), "' must be of length ",
         deparse(substitute(n)), call. = FALSE)
  }
  
  invisible(NULL)  
}
