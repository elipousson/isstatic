#' Is the length of x between two values?
#'
#' @param x Object to check.
#' @param left,right Min and max values to check if the length of x is between.
#' @noRd
has_len_between <- function(x, left, right) {
  len <- length(x)
  (len >= left) && (len <= right)
}

#' Do two object have an identical length?
#'
#' @param x,y Two strings or character vectors to compare.
#' @param ... Additional parameters passed to [identical()]
#' @noRd
has_same_len <- function(x, y, ...) {
  identical(length(x), length(y), ...)
}
