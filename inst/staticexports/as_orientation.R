#' What is the orientation of a numeric aspect ratio?
#'
#' @param x A numeric vector with an aspect ratio or a data.frame with width and
#'   height column (using width and height values from columns matching the cols
#'   parameter).
#' @param tolerance Positive numeric value above or below 1 used to determine if
#'   an aspect ratio is square, landscape, or portrait.
#' @param cols Name of width and height column if x is a data.frame object.
#' @returns A character vector of orientations of the same length as x or, if x
#'   is a data.frame, the same length as the number of rows in x.
#' @noRd
as_orientation <- function(x, tolerance = 0.1, cols = c("width", "height")) {
  tolerance <- abs(tolerance)

  if (is.data.frame(x)) {
    static_check_name(x, cols)
    return(
      as_orientation(
        as.numeric(x[, cols[1]]) / as.numeric(x[, cols[2]]),
        tolerance
      )
    )
  }

  static_check_numeric(x)

  if (length(x) > 1) {
    return(map_chr(x, as_orientation, tolerance))
  }

  if (x > (1 + tolerance)) {
    return("landscape")
  }

  if (x < (1 - tolerance)) {
    return("portrait")
  }

  "square"
}
