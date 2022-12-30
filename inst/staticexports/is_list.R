#' Do all items in this list inherit the provided class?
#'
#' @param x Object to be tested.
#' @param what A character vector naming classes.
#' @noRd
is_list_of <- function(x, what = NULL) {
  is.list(x) && all(vapply(x, FUN = inherits, FUN.VALUE = TRUE, what))
}


#' Do all items in this list inherit the gg class?
#'
#' @param x Object to be tested.
#' @noRd
is_gg_list <- function(x) {
  is_list_of(x, "gg")
}


#' Do all items in this list inherit the sf class?
#'
#' @param x Object to be tested.
#' @noRd
is_sf_list <- function(x) {
  is_list_of(x, "sf")
}
