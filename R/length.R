# `R/length.R` is imported from `inst/staticexports/length.R`. 
# Please edit that file instead.

#' Is the length of x between two values?
#'
#' @param x Object to check.
#' @param left,right Min and max values to check if the length of x is between.
#' @param min Min value used by [isstatic::has_min_length()].
#' @param max Max value used by [isstatic::has_max_length()].
#' @name has_len_between
#' @export
has_len_between <- function(x, left = 1, right = left) {
  len <- length(x)
  (len >= left) && (len <= right)
}

#' @name has_min_length
#' @rdname has_len_between
#' @export
has_min_length <- function(x, min) {
  length(x) >= min
}

#' @name has_max_length
#' @rdname has_len_between
#' @export
has_max_length <- function(x, max) {
  length(x) <= max
}

#' Do two object have an identical length?
#'
#' @param x,y Two strings or character vectors to compare.
#' @param ... Additional parameters passed to [identical()]
#' @export
has_same_len <- function(x, y, ...) {
  identical(length(x), length(y), ...)
}
