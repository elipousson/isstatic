#' Is this a gg class object?
#'
#' @param x Object to be tested.
#' @noRd
is_gg <- function(x) {
  inherits(x, "gg")
}


#' Is this a ggplot class object?
#'
#' @name is_ggplot
#' @rdname is_gg
#' @noRd
is_ggplot <- function(x) {
  inherits(x, "ggplot")
}


#' Is this a ggproto class object?
#'
#' @name is_ggproto
#' @rdname is_gg
#' @noRd
is_ggproto <- function(x) {
  inherits(x, "ggproto")
}

#' Is this a patchwork class object?
#'
#' @name is_patchwork
#' @rdname is_gg
#' @noRd
is_patchwork <- function(x) {
  inherits(x, "patchwork")
}
