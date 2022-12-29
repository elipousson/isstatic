# `R/is_named.R` is imported from `inst/staticexports/is_named.R`. 
# Please edit that file instead.

#' Is a named list or character vector?
#'
#' @param x A data frame or another named object.
#' @export
is_named <- function (x) {
  !is.null(names(x)) && !any("" %in% names(x))
}


#' Does an object have all of the provided names?
#'
#' @param x A data frame or another named object.
#' @param name Element name(s) to check.
#' @export
has_all_names <- function(x, name) {
  if (anyNA(c(x, name))) {
    return(FALSE)
  }

  all(name %in% names(x), na.rm = TRUE)
}


#' Is a named list or character vector?
#'
#' @param x A data frame or another named object.
#' @param name Element name(s) to check.
#' @export
has_any_names <- function(x, name) {
  if (anyNA(c(x, name))) {
    return(FALSE)
  }

  any(name %in% names(x), na.rm = TRUE)
}
