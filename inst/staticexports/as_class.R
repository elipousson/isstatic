#' Coerce a sf object to a sfc object
#'
#' If possible, function should be updated to support bbox objects or other
#' spatial data classes.
#'
#' @param x A sf object to coerce.
#' @noRd
as_sfc <- function(x) {
  if (is_sfc(x)) {
    return(x)
  }

  if (!is_sf(x)) {
    stop("as_sfc only supports sf objects")
  }

  x[[attributes(x)[["sf_column"]]]]
}
