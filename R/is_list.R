# `R/is_list.R` is imported from `inst/staticexports/is_list.R`. 
# Please edit that file instead.

#' Do all items in this list inherit the provided class?
#'
#' @param x Object to be tested.
#' @param what A character vector naming classes.
#' @export
is_list_all <- function(x, what = NULL) {
  is.list(x) && all(vapply(x, FUN = inherits, FUN.VALUE = TRUE, what))
}

#' Do all items in this list inherit the gg class?
#'
#' @param x Object to be tested.
#' @export
is_gg_list <- function(x) {
  is_list_all(x, "gg")
}

#' Do all items in this list inherit the sf class?
#'
#' @param x Object to be tested.
#' @export
is_sf_list <- function(x) {
  is_list_all(x, "sf")
}
