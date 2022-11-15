# `R/has.R` is imported from `inst/staticexports/has.R`. 
# Please edit that file instead.

#' Do two object have an identical length?
#'
#' @param x,y Two strings or character vectors to compare.
#' @param ... Additional parameters passed to [identical()]
#' @export
has_same_len <- function(x, y, ...) {
  identical(length(x), length(y), ...)
}
