# `R/is_url.R` is imported from `inst/staticexports/is_url.R`. 
# Please edit that file instead.

#' Is a character vector a URL?
#'
#' @param x Object to be tested.
#' @export
is_url <- function(x) {
  grepl(
    "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+",
    x
  )
}

#' Is a character vector an ArcGIS MapServer or FeatureServer URL?
#'
#' @param x Object to be tested.
#' @export
is_esri_url <- function(x) {
  grepl("/MapServer|/FeatureServer", x)
}

#' Is a character vector a Google Sheets URL?
#'
#' @param x Object to be tested.
#' @export
is_gsheet_url <- function(x) {
  grepl("^https://docs.google.com/spreadsheets/", x)
}

#' Is a character vector a URL for a GitHub Gist?
#'
#' @param x Object to be tested.
#' @export
is_gist_url <- function(x) {
  grepl("^https://gist.github.com/", x)
}

#' Is a character vector a Google Maps URL?
#'
#' @param x Object to be tested.
#' @export
is_gmap_url <- function(x) {
  grepl("^https://www.google.com/maps/", x)
}
