#' Does this text end in the provided file extension?
#'
#' @param x A character vector to check for matches, or an object which can be
#'   coerced by [as.character()] to a character vector.
#' @param fileext A file extension to compare to x. Required. If a vector of
#'   multiple extensions are provided, returns `TRUE` for any match.
#' @inheritParams base::grepl
#' @seealso [isstatic::has_fileext()]
#' @noRd
is_fileext_path <- function(x, fileext, ignore.case = TRUE) {
  grepl(
    paste0("\\.", paste0(fileext, collapse = "|"), "$(?!\\.)"),
    x,
    ignore.case = ignore.case, perl = TRUE
  )
}

#' Does this text end in the specified filetype?
#'
#' @inheritParams is_fileext_path
#' @param filetype A file extension (or multiple file extensions) to compare to
#'   x. Required.
#' @noRd
is_filetype_path <- function(x, filetype, ignore.case = TRUE) {
  is_fileext_path(x, filetype, ignore.case)
}

#' [is_geojson_fileext]: Does this text end with a GeoJSON file extension?
#'
#' @name is_geojson_fileext
#' @rdname is_fileext_path
#' @noRd
is_geojson_fileext <- function(x, ignore.case = TRUE) {
  is_fileext_path(x, "geojson", ignore.case)
}

#' [is_csv_fileext]: Does this text end with a CSV file extension?
#'
#' @name is_csv_fileext
#' @rdname is_fileext_path
#' @noRd
is_csv_fileext <- function(x, ignore.case = TRUE) {
  is_fileext_path(x, "csv", ignore.case)
}

#' [is_excel_fileext]: Does this text end with a XLS or XLSX file extension?
#'
#' @name is_excel_fileext
#' @rdname is_fileext_path
#' @noRd
is_excel_fileext <- function(x, ignore.case = TRUE) {
  is_fileext_path(x, c("xls", "xlsx"), ignore.case)
}

#' [is_rdata_fileext]: Does this text end with a rds, rda, or RData file extension?
#'
#' @name is_rdata_fileext
#' @rdname is_fileext_path
#' @noRd
is_rdata_fileext <- function(x, ignore.case = TRUE) {
  any(
    c(
      is_rda_fileext(x, ignore.case),
      is_rds_fileext(x, ignore.case),
      is_fileext_path(x, "RData", ignore.case)
    )
  )
}

#' [is_rds_fileext]: Does this text end with a rds file extension?
#'
#' @name is_rds_fileext
#' @rdname is_fileext_path
#' @noRd
is_rds_fileext <- function(x, ignore.case = TRUE) {
  is_fileext_path(x, "rds", ignore.case)
}

#' [is_rda_fileext]: Does this text end with a rda file extension?
#'
#' @name is_rda_fileext
#' @rdname is_fileext_path
#' @noRd
is_rda_fileext <- function(x, ignore.case = TRUE) {
  is_fileext_path(x, "rda", ignore.case)
}

#' [is_zip_fileext]: Does this text end with a zip file extension?
#'
#' @name is_zip_fileext
#' @rdname is_fileext_path
#' @noRd
is_zip_fileext <- function(x, ignore.case = TRUE) {
  is_fileext_path(x, "zip", ignore.case)
}

