
#' Coerce a sf, sfc, or bbox object to a coordinate reference system
#'
#' This function should be updated to support stars and terra objects.
#'
#' @param x A sf, sfc, or bbox object to coerce into a CRS.
#' @param input If `TRUE` (default), return only the "input" component of the
#'   crs object. If `FALSE`, return the full crs object.
#' @noRd
as_crs <- function(x, input = TRUE) {
  #  Previously: Character strings starting with EPSG: or ESRI: are returned as
  #  is. Numeric vectors are returned as is without checking and may or may not
  #  have been converted to a crs.

  # if ((is.character(x) && grepl("^(EPSG|ESRI):", x)) | is.numeric(x)) {
  #   return(x)
  # }

  if (is_sf(x)) {
    x <- as_sfc(x)
  }

  check_if(
    is_sfc(x) | is_bbox(x),
    "`as_crs()` requires a <sf>, <sfc>, or <bbox> object."
  )

  crs <- attributes(x)[["crs"]]

  if (is.na(crs)) {
    return(NA)
  }

  if (isTRUE(input)) {
    return(crs[["input"]])
  }

  crs
}

#' - [is_lonlat_crs()]: Is this sf, sfc, or bbox class object with a geographic coordinate reference
#' system ("EPSG:4326" or "EPSG:4269")?
#'
#' @name is_lonlat_crs
#' @rdname as_crs
#' @noRd
is_lonlat_crs <- function(x, crs = c("EPSG:4326", "EPSG:4269")) {
  if (!is_sf_ext(x)) {
    return(FALSE)
  }

  as_crs(x) %in% crs
}

#' - [has_same_crs()]: Do two sf, sfc, or bbox objects use the same coordinate
#' reference system?
#'
#' @param x,y sf, sfc, or bbox objects to be compared.
#' @param ... Additional parameters passed to [identical()] by [has_same_crs()].
#' @noRd
has_same_crs <- function(x, y, ...) {
  identical(as_crs(x), as_crs(y), ...)
}
