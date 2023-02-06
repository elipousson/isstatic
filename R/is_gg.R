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
#' @name is_ggplot
#' @rdname is_gg
#' @export
is_ggplot <- function(x) {
  inherits(x, "ggplot")
}


#' Is this a ggproto class object?
#'
#' @name is_ggproto
#' @rdname is_gg
#' @export
is_ggproto <- function(x) {
  inherits(x, "ggproto")
}

#' Is this a patchwork class object?
#'
#' @name is_patchwork
#' @rdname is_gg
#' @export
is_patchwork <- function(x) {
  inherits(x, "patchwork")
}
