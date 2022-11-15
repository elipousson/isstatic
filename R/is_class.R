# `R/is_class.R` is imported from `inst/staticexports/is_class.R`. 
# Please edit that file instead.

#' Is this a units class object?
#'
#' @export
is_units <- function(x) {
  inherits(x, "units")
}

#' Is this a unit class object?
#'
#' @export
is_unit <- function(x) {
  inherits(x, c("unit", "unit_v2"))
}
