# Join multiple strings into a single string

Dependency-free drop-in alternative for `stringr::str_c()`.

## Usage

``` r
str_c(..., sep = "", collapse = NULL)
```

## Source

Adapted from the [stringr](https://stringr.tidyverse.org/) package.

## Arguments

- ...:

  One or more character vectors. Zero length arguments are removed.
  Short arguments are recycled to the length of the longest.

  Like most other R functions, missing values are "infectious": whenever
  a missing value is combined with another string the result will always
  be missing. Use `str_replace_na()` to convert `NA` to "NA"

- sep:

  String to insert between input vectors.

- collapse:

  Optional string used to combine input vectors into single string.

## Value

If `collapse = NULL` (the default) a character vector with length equal
to the longest input string. If collapse is non-`NULL`, a character
vector of length 1.
