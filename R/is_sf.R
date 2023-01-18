# `R/is_sf.R` is imported from `inst/staticexports/is_sf.R`. 
# Please edit that file instead.

#' Is this a sf class object?
#'
#' @param x An object to be tested with [inherits()]
#' @seealso [isstatic::as_crs()]
#' @export
is_sf <- function(x) {
  inherits(x, "sf")
}


#' Is this a sfc class object?
#'
#' @name is_sfc
#' @rdname is_sf
#' @export
is_sfc <- function(x) {
  inherits(x, "sfc")
}


#' Is this a sfg class object?
#'
#' @name is_sfg
#' @rdname is_sf
#' @export
is_sfg <- function(x) {
  inherits(x, "sfg")
}


#' Is this a bbox class object?
#'
#' @name is_bbox
#' @rdname is_sf
#' @export
is_bbox <- function(x) {
  inherits(x, "bbox")
}

#' Is this a sf, sfc, or bbox class object?
#'
#' @name is_sf_ext
#' @rdname is_sf
#' @param ext If `TRUE`, return `TRUE` is x is a sf, sfc, or bbox object. If
#'   `FALSE`, only check if x is an sf object. If ext is a character object, it
#'   is passed to the what parameter of [inherits()] with sf.
#' @export
is_sf_ext <- function(x, ext = TRUE) {
  if (is.logical(ext)) {
    if (!isTRUE(ext)) {
      return(is_sf(x))
    }

    ext <- c("sfc", "bbox")
  }

  inherits(x, c("sf", ext))
}


#' Is this a RasterLayer class object?
#'
#' @name is_raster
#' @rdname is_sf
#' @export
is_raster <- function(x) {
  inherits(x, "RasterLayer")
}


#' Is this a Spatial class (sp) object?
#'
#' @name is_sp
#' @rdname is_sf
#' @export
is_sp <- function(x) {
  inherits(x, "Spatial")
}
