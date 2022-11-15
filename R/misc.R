# `R/misc.R` is imported from `inst/staticexports/misc.R`. 
# Please edit that file instead.

#' Replace spaces in string with underscores
#'
#' @param string String to transform by replacing spaces with underscores.
#' @export
underscore <- function(string) {
  gsub(" ", "_", string)
}
