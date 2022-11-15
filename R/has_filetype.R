# `R/has_filetype.R` is imported from `inst/staticexports/has_filetype.R`. 
# Please edit that file instead.

#' Does string contain a filetype or the specified filetype?
#'
#' @param string String to be tested with or without filetype.
#' @param filetype File type to test against. Optional.
#' @param ignore.case If `FALSE`, the pattern matching is case sensitive. If
#'   `TRUE`, case is ignored.
#' @export
has_filetype <- function(string, filetype = NULL, ignore.case = FALSE) {
  if (is.null(filetype)) {
    filetype <- "[a-zA-Z0-9]+"
  }
  grepl(paste0("\\.", filetype, "$(?!\\.)"), string, ignore.case, perl = TRUE)
}
