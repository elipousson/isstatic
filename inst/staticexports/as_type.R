#' as.integer with option to suppress warnings for NA coercion
#'
#' @inheritParams base::as.integer
#' @param quiet If `TRUE`, suppress warnings about creation of NA values through
#'   coercion of object types. Default to `TRUE`.
#' @noRd
as_integer <- function(x, quiet = TRUE) {
  if (isTRUE(quiet)) {
    return(suppressWarnings(as.integer(x)))
  }

  as.integer(x)
}

#' as.roman with option to suppress warnings for NA coercion
#'
#' @inheritParams utils::as.roman
#' @param quiet If `TRUE`, suppress warnings about creation of NA values through
#'   coercion of object types. Default to `TRUE`.
#' @noRd
as_roman <- function(x, quiet = TRUE) {
  if (isTRUE(quiet)) {
    return(suppressWarnings(utils::as.roman(x)))
  }

  utils::as.roman(x)
}
