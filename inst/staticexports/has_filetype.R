#' Does string contain the specified file type or any file extension?
#'
#' Check if string contains any filetype or the provided filetype. If string is
#' `NULL`, returns `FALSE`.
#'
#' @param string String to be tested with or without filetype. Defaults to
#'   `NULL`.
#' @param fileext File type to test against. Optional.
#' @param ignore.case If `FALSE`, the pattern matching is case sensitive. If
#'   `TRUE`, case is ignored.
#' @seealso [isstatic::is_fileext_path()]
#' @noRd
has_fileext <- function(string = NULL, fileext = NULL, ignore.case = FALSE) {
  if (is.null(string)) {
    return(FALSE)
  }

  if (is.null(fileext)) {
    fileext <- "[a-zA-Z0-9]+"
  }

  is_fileext_path(string, fileext, ignore.case)
}

#' Does string contain the specified file type or any file extension?
#'
#' Alternate naming convention for [isstatic::has_fileext()]
#'
#' @noRd
has_filetype <- function(string = NULL, filetype = NULL, ignore.case = FALSE) {
  has_fileext(string, filetype, ignore.case)
}
