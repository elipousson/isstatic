# `R/is_gg.R` is imported from `inst/staticexports/is_gg.R`. 
# Please edit that file instead.

#' Is this a gg class object?
#'
#' @param x Object to be tested.
#' @export
is_gg <- function(x) {
  inherits(x, "gg")
}


#' Is this a ggplot class object?
#'
#' @param x Object to be tested.
#' @export
is_ggplot <- function(x) {
  inherits(x, "ggplot")
}


#' Is this a ggproto class object?
#'
#' @param x Object to be tested.
#' @export
is_ggproto <- function(x) {
  inherits(x, "ggproto")
}


#' Is this a patchwork class object?
#'
#' @param x Object to be tested.
#' @export
is_patchwork <- function(x) {
  inherits(x, "patchwork")
}
