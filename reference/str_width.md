# Compute the width of a string

Dependency-free drop-in alternative for `stringr::str_width()`. Results
for non-ASCII characters may be inaccurate in R \< 4.0.

## Usage

``` r
str_width(string)
```

## Source

Adapted from the [stringr](https://stringr.tidyverse.org/) package.

## Arguments

- string:

  Input vector. Either a character vector, or something coercible to
  one.

## Value

A numeric vector the same length as string.
