% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stringstatic.R
\name{str_c}
\alias{str_c}
\title{Join multiple strings into a single string}
\source{
Adapted from the \href{https://stringr.tidyverse.org/}{stringr} package.
}
\usage{
str_c(..., sep = "", collapse = NULL)
}
\arguments{
\item{...}{One or more character vectors.
Zero length arguments are removed.
Short arguments are recycled to the length of the longest.

Like most other R functions, missing values are "infectious":
whenever a missing value is combined with another string
the result will always be missing.
Use \code{str_replace_na()} to convert \code{NA} to "NA"}

\item{sep}{String to insert between input vectors.}

\item{collapse}{Optional string used to combine input vectors into single string.}
}
\value{
If \code{collapse = NULL} (the default) a character vector
with length equal to the longest input string.
If collapse is non-\code{NULL}, a character vector of length 1.
}
\description{
Dependency-free drop-in alternative for \code{stringr::str_c()}.
}
