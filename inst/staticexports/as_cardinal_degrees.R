cardinal_degrees <-
  c(
    "N" = 0, "N" = 360, "E" = 90,
    "S" = 180, "W" = 270,
    "NE" = 45, "SE" = 135,
    "SW" = 225, "NW" = 315,
    "NNE" = 22.5, "ENE" = 67.5, "ESE" = 112.5,
    "SSE" = 157.5, "SSW" = 202.5, "WSW" = 247.5,
    "WNW" = 292.5, "NNW" = 337.5
  )


#' What are the closest cardinal degrees to a numeric degree value?
#'
#' @param x A numeric vector with degrees.
#' @param winds Number of winds to use for results (4, 8, or 16).
#' @returns A named numeric vector with cardinal degrees and wind names.
#' @noRd
as_cardinal_degrees <- function(x, winds = 8) {
  stopifnot(
    "winds must be 4, 8, or 16." = winds %in% c(4, 8, 16),
    "x must be a numeric vector." = is.numeric(x)
    )

  wind_degrees <- sort(cardinal_degrees[c(1:(winds + 1))])

  sapply(
    x,
    function(i) {
      wind_degrees[findInterval(i, wind_degrees - (360 / (winds * 2)))]
    }
  )
}
