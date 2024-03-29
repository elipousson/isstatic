% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/is_named.R
\name{is_named}
\alias{is_named}
\alias{has_all_names}
\alias{has_any_names}
\title{Is this object a named list or character vector?}
\usage{
is_named(x)

has_all_names(x, name)

has_any_names(x, name)
}
\arguments{
\item{x}{A data frame or another named object.}

\item{name}{Element name(s) to check.}
}
\description{
Is this object a named list or character vector?

Does this object have all of the provided names?

Does this object have any of the provided names?
}
\seealso{
\code{\link[rlang:is_named]{rlang::is_named()}}
}
