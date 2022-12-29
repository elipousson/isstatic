#' Is this a units class object?
#'
#' @param x Object to be tested.
#' @noRd
is_units <- function(x) {
  inherits(x, "units")
}

#' Is this a unit class object?
#'
#' @param x Object to be tested.
#' @noRd
is_unit <- function(x) {
  inherits(x, "unit")
}
