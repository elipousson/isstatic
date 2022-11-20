# `R/has_filetype.R` is imported from `inst/staticexports/has_filetype.R`. 
# Please edit that file instead.

#' Does string contain the specified file type or any file extension?
#'
#' Check if string contains any filetype or the provided filetype. If string is
#' `NULL`, returns `FALSE`.
#'
#' @param string String to be tested with or without filetype. Defaults to
#'   `NULL`.
#' @param filetype File type to test against. Optional.
#' @param ignore.case If `FALSE`, the pattern matching is case sensitive. If
#'   `TRUE`, case is ignored.
#' @export
has_filetype <- function(string = NULL, filetype = NULL, ignore.case = FALSE) {
  if (is.null(string)) {
    return(FALSE)
  }

  if (is.null(filetype)) {
    filetype <- "[a-zA-Z0-9]+"
  }

  is_filetype_path(string, filetype, ignore.case)
}
