# Remove matched patterns in a string

Dependency-free drop-in alternative for `stringr::str_remove()`.

## Usage

``` r
str_remove(string, pattern)
```

## Source

Adapted from the [stringr](https://stringr.tidyverse.org/) package.

## Arguments

- string:

  Input vector. Either a character vector, or something coercible to
  one.

- pattern:

  Pattern to look for.

  The default interpretation is a regular expression, as described in
  [base::regex](https://rdrr.io/r/base/regex.html). Control options with
  [`regex()`](https://rdrr.io/r/base/regex.html).

  Match a fixed string (i.e. by comparing only bytes), using `fixed()`.
  This is fast, but approximate.

## Value

A character vector.
