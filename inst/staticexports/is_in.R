#' Are any of x in y?
#'
#' @param x Object to be tested.
#' @param y Vector to compare x to.
#' @noRd
is_any_in <- function(x, y) {
  any(x %in% y)
}

#' Are none of x in y?
#'
#' @param x Object to be tested.
#' @param y Vector to compare x to.
#' @noRd
is_none_in <- function(x, y) {
  isFALSE(is_any_in(x, y))
}

#' Is all of x in y?
#'
#' @param x Object to be tested.
#' @param y Vector to compare x to.
#' @noRd
is_all_in <- function(x, y) {
  all(x %in% y)
}
