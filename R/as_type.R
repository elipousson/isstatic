# `R/as_type.R` is imported from `inst/staticexports/as_type.R`. 
# Please edit that file instead.

#' as.integer with option to suppress warnings for NA coercion
#'
#' @inheritParams base::as.integer
#' @export
as_integer <- function(x, quiet = TRUE) {
  if (isTRUE(quiet)) {
    return(suppressWarnings(as.integer(x)))
  }

  as.integer(x)
}

#' as.roman with option to suppress warnings for NA coercion
#'
#' @inheritParams utils::as.roman
#' @export
as_roman <- function(x, quiet = TRUE) {
  if (isTRUE(quiet)) {
    return(suppressWarnings(utils::as.roman(x)))
  }

  utils::as.roman(x)
}
