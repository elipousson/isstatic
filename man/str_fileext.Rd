% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/str_fileext.R
\name{str_fileext}
\alias{str_fileext}
\alias{str_add_fileext}
\alias{str_remove_fileext}
\alias{str_extract_fileext}
\title{Add, remove, or extract file extensions from character vectors}
\usage{
str_add_fileext(string, fileext = NULL)

str_remove_fileext(string, fileext = NULL)

str_extract_fileext(string, fileext = NULL)
}
\arguments{
\item{string}{Character vector of any length. Required.}

\item{fileext}{File extension. Optional. Defaults to \code{NULL}.}
}
\description{
These function uses \code{\link[stringstatic:str_c]{stringstatic::str_c()}}, \code{\link[stringstatic:str_remove]{stringstatic::str_remove()}} and
\code{\link[stringstatic:str_extract]{stringstatic::str_extract()}} and works to:
\itemize{
\item Add file extensions (or replace existing file extensions) with
\code{\link[=str_add_fileext]{str_add_fileext()}}
\item Remove file extensions with \code{\link[=str_remove_fileext]{str_remove_fileext()}}
\item Extract existing file names \code{\link[=str_extract_fileext]{str_extract_fileext()}} (returning NA values if
a string has no file extension)
}
}
\examples{
str_add_fileext("image", "jpeg")

str_remove_fileext(c("file.txt", "word.docx"), "docx")

str_extract_fileext(c("file1.pdf", "file2"))

str_extract_fileext(c("image1.png", "image2.jpeg"), "jpeg")
}
\seealso{
\itemize{
\item \code{\link[=has_fileext]{has_fileext()}}
\item \code{\link[=is_fileext_path]{is_fileext_path()}}
}
}
