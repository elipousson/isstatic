# `R/is_in.R` is imported from `inst/staticexports/is_in.R`. 
# Please edit that file instead.

#' Are any of x in y?
#'
#' @param x Object to be tested.
#' @param y Vector to compare x to.
#' @export
is_any_in <- function(x, y) {
  any(x %in% y)
}

#' Are none of x in y?
#'
#' @param x Object to be tested.
#' @param y Vector to compare x to.
#' @export
is_none_in <- function(x, y) {
  !any(x %in% y)
}

#' Is all of x in y?
#'
#' @param x Object to be tested.
#' @param y Vector to compare x to.
#' @export
is_all_in <- function(x, y) {
  all(x %in% y)
}
