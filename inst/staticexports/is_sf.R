#' Is this a sf class object?
#'
#' @param x Object to be tested.
#' @noRd
is_sf <- function(x) {
  inherits(x, "sf")
}


#' Is this a sfc class object?
#'
#' @param x Object to be tested.
#' @noRd
is_sfc <- function(x) {
  inherits(x, "sfc")
}


#' Is this a sfg class object?
#'
#' @param x Object to be tested.
#' @noRd
is_sfg <- function(x) {
  inherits(x, "sfg")
}


#' Is this a bbox class object?
#'
#' @param x Object to be tested.
#' @noRd
is_bbox <- function(x) {
  inherits(x, "bbox")
}

#' Is this a sf, sfc, or bbox class object?
#'
#' @param x Object to be tested.
#' @param ext If `TRUE`, return `TRUE` is x is a sf, sfc, or bbox object. If
#'   `FALSE`, only check if x is an sf object. If ext is a character object, it
#'   is passed to the what parameter of [inherits()] with sf.
#' @noRd
is_sf_ext <- function(x, ext = TRUE) {
  if (is.logical(ext)) {
    if (ext) {
      ext <- c("sfc", "bbox")
    } else {
      ext <- NULL
    }
  }

  inherits(x, c("sf", ext))
}


#' Is this a RasterLayer class object?
#'
#' @param x Object to be tested.
#' @export
is_raster <- function(x) {
  inherits(x, "RasterLayer")
}


#' Is this a Spatial class (sp) object?
#'
#' @param x Object to be tested.
#' @export
is_sp <- function(x) {
  inherits(x, "Spatial")
}


#' Is this sf, sfc, or bbox class object in a geographic coordinate system?
#'
#' This function currently only checks for "EPSG:4326" and "EPSG:4269" CRS.
#'
#' @param x Object to be tested.
#' @noRd
is_lonlat <- function(x) {
  if (inherits(x, c("sf", "sfc", "bbox"))) {
    grepl(
      as_crs(x),
      c("EPSG:4326", "EPSG:4269")
    )
  } else {
    FALSE
  }
}

#' Do two sf, sfc, or bbox objects use the same coordinate reference system?
#'
#' @param x,y sf, sfc, or bbox objects to be compared.
#' @param ... Additional parameters passed to [identical()]
#' @noRd
has_same_crs <- function(x, y, ...) {
  identical(as_crs(x), as_crs(y), ...)
}

#' Coerce a sf, sfc, or bbox object to a coordinate reference system
#'
#' This function should be updated to support stars objects.
#'
#' @param x A sf, sfc, or bbox object to coerce into a CRS. Character strings
#'   starting with EPSG: or ESRI: are returned as is. Numeric vectors are
#'   returned as is without checking.
#' @param input If `TRUE` (default), return only the "input" component of the
#'   crs object. If `FALSE`, return the full crs object.
#' @noRd
as_crs <- function(x, input = TRUE) {
  if ((is.character(x) && grepl("^(EPSG|ESRI):", x)) | is.numeric(x)) {
    return(x)
  }

  if (is_sf(x)) {
    x <- as_sfc(x)
  }

  if (!is_sfc(x) && !is_bbox(x)) {
    stop(
      "`as_crs()` only supports sf, sfc, and bbox objects."
    )
  }

  crs <- attributes(x)[["crs"]]

  if (is.na(crs)) {
    return(NA)
  }

  if (input) {
    return(crs[["input"]])
  }

  crs
}
