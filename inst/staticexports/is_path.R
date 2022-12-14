#' Is this a file path or url ending in the specified file extension?
#'
#' @param x A character vector to check.
#' @param fileext A file extension (or multiple file extensions) to compare to
#'   x. Required.
#' @inheritParams base::grepl
#' @noRd
is_fileext_path <- function(x, fileext, ignore.case = TRUE) {
  grepl(
    paste0("\\.", paste0(fileext, collapse = "|"), "$(?!\\.)"),
    x,
    ignore.case = ignore.case, perl = TRUE
  )
}

#' Is this a file path or url ending in the specified filetype?
#'
#' @inheritParams is_fileext_path
#' @param filetype A file extension (or multiple file extensions) to compare to
#'   x. Required.
#' @noRd
is_filetype_path <- function(x, filetype, ignore.case = TRUE) {
  is_fileext_path(x, filetype, ignore.case)
}

#' Is this a GeoJSON file path or url?
#'
#' @inheritParams is_fileext_path
#' @noRd
is_geojson_path <- function(x, ignore.case = TRUE) {
  is_fileext_path(x, "geojson", ignore.case)
}

#' Is this a CSV file path or url?
#'
#' @inheritParams is_fileext_path
#' @noRd
is_csv_path <- function(x, ignore.case = TRUE) {
  is_fileext_path(x, "csv", ignore.case)
}

#' Is this a Excel file path or url?
#'
#' @inheritParams is_fileext_path
#' @noRd
is_excel_path <- function(x, ignore.case = TRUE) {
  is_fileext_path(x, c("xls", "xlsx"), ignore.case)
}

#' Is this a RDS, RDA, or RData file path or url?
#'
#' @inheritParams is_fileext_path
#' @noRd
is_rdata_path <- function(x, ignore.case = TRUE) {
  any(
    c(
      is_rda_path(x, ignore.case),
      is_rds_path(x, ignore.case),
      is_fileext_path(x, "RData", ignore.case)
    )
  )
}

#' Is this a RDS file path or url?
#'
#' @inheritParams is_fileext_path
#' @noRd
is_rds_path <- function(x, ignore.case = TRUE) {
  is_fileext_path(x, "rds", ignore.case)
}

#' Is this a RDA file path or url?
#'
#' @inheritParams is_fileext_path
#' @noRd
is_rda_path <- function(x, ignore.case = TRUE) {
  is_fileext_path(x, "rda", ignore.case)
}
