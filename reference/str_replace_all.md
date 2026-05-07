# Replace matched patterns in a string

Dependency-free drop-in alternative for `stringr::str_replace_all()`.

## Usage

``` r
str_replace_all(string, pattern, replacement)
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

- replacement:

  A character vector of replacements. Should be either length one, or
  the same length as `string` or `pattern`. References of the form `\1`,
  `\2`, etc. will be replaced with the contents of the respective
  matched group (created by `()`).

  To perform multiple replacements in each element of `string`, pass a
  named vector `(c(pattern1 = replacement1))` to `str_replace_all()`.

  To replace the complete string with `NA`, use
  `replacement = NA_character_`.

  Using a function for `replacement` is not yet supported.

## Value

A character vector.
