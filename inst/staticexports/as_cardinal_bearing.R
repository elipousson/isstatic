cardinal_bearings <-
  c(
    "N" = 0, "N" = 360, "E" = 90,
    "S" = 180, "W" = 270,
    "NE" = 45, "SE" = 135,
    "SW" = 225, "NW" = 315,
    "NNE" = 22.5, "ENE" = 67.5, "ESE" = 112.5,
    "SSE" = 157.5, "SSW" = 202.5, "WSW" = 247.5,
    "WNW" = 292.5, "NNW" = 337.5
  )


#' Convert a numeric bearing value to the closest cardinal bearing
#'
#' @param x A numeric vector with degrees or a data.frame with column name
#'   matching the first name in cols.
#' @param winds Number of winds to use for results (4, 8, or 16).
#' @param cols A length 2 character vector where the first value is a column
#'   name containing bearing values and the second is the name of the new column
#'   added to the data.frame. Required if x is a data.frame.
#' @returns A named numeric vector with cardinal bearings (and wind names) or a
#'   data.frame with an added column containing the cardinal bearings.
#' @noRd
as_cardinal_bearing <- function(x,
                                winds = 8,
                                cols = c("bearing", "cardinal_bearing")) {
  if (is.data.frame(x)) {
    static_check_name(x, cols[1])
    x[[cols[2]]] <- as_cardinal_bearing(x[[cols[1]]], winds)
    return(x)
  }

  static_check_numeric(x)
  static_check_if(
    condition = winds %in% c(4, 8, 16),
    "`winds` must be 4, 8, or 16."
  )

  wind_degrees <- sort(cardinal_bearings[c(1:(winds + 1))])

  sapply(
    x,
    function(i) {
      wind_degrees[findInterval(i, wind_degrees - (360 / (winds * 2)))]
    }
  )
}
