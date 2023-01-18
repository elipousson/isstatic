# `R/is_url.R` is imported from `inst/staticexports/is_url.R`. 
# Please edit that file instead.

#' Is an object a URL?
#'
#' @param x A object to be tested.
#' @export
is_url <- function(x) {
  grepl(
    "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+",
    x
  )
}

#' - [is_esri_url()]: Is an object an ArcGIS MapServer or FeatureServer URL?
#'
#' @name is_esri_url
#' @rdname is_url
#' @export
is_esri_url <- function(x) {
  is_url(x) && grepl("/MapServer|/FeatureServer", x)
}

#' - [is_gsheet_url()]: Is an object a Google Sheets URL?
#'
#' @name is_gsheet_url
#' @rdname is_url
#' @export
is_gsheet_url <- function(x) {
  grepl("^https://docs.google.com/spreadsheets/", x)
}

#' - [is_gist_url()]: Is an object a URL for a GitHub Gist?
#'
#' @name is_gist_url
#' @rdname is_url
#' @export
is_gist_url <- function(x) {
  grepl("^https://gist.github.com/", x)
}

#' - [is_gmap_url()]: Is an object a Google Maps URL?
#'
#' @name is_gmap_url
#' @rdname is_url
#' @export
is_gmap_url <- function(x) {
  grepl("^https://www.google.com/maps/", x)
}
