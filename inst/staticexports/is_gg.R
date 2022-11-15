#' Is this a gg class object?
#'
#' @param x Object to be tested.
#' @noRd
is_gg <- function(x) {
  inherits(x, "gg")
}


#' Is this a ggplot class object?
#'
#' @param x Object to be tested.
#' @noRd
is_ggplot <- function(x) {
  inherits(x, "ggplot")
}


#' Is this a ggproto class object?
#'
#' @param x Object to be tested.
#' @noRd
is_ggproto <- function(x) {
  inherits(x, "ggproto")
}

