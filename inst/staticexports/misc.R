#' Replace spaces in string with underscores
#'
#' @param string String to transform by replacing spaces with underscores.
#' @noRd
underscore <- function(string) {
  gsub(" ", "_", string)
}
