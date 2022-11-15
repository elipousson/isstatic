# `R/is_misc.R` is imported from `inst/staticexports/is_misc.R`. 
# Please edit that file instead.

#' Is this a units class object?
#'
#' @param x Object to be tested.
#' @export
is_units <- function(x) {
  inherits(x, "units")
}

#' Is this a unit class object?
#'
#' @param x Object to be tested.
#' @export
is_unit <- function(x) {
  inherits(x, c("unit", "unit_v2"))
}

#' Is a named list or character vector?
#'
#' @param x Object to be tested.
#' @export
is_named <- function (x) {
  !is.null(names(x)) && !any("" %in% names(x))
}
