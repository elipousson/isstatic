# Modify digits within strings

`str_replace_digits()`: Replace digits with a string optionally
incrementing the digits `str_pad_digits()`: Pad a string with digits
`str_extract_digits()`: Extract digits from a string

## Usage

``` r
str_pad_digits(string, pad = "0", side = "left", width = NULL)

str_extract_digits(string, pattern = "[0-9]+", side = NULL)

str_replace_digits(string, replacement, pad = "0", side = "left", width = NULL)

str_increment_digits(string, increment = TRUE, ...)
```

## Arguments

- string:

  Input vector. Either a character vector, or something coercible to
  one.

- pad:

  Single padding character added to digits in string; defaults to "0"

- side:

  Side on which padding character is added (left, right or both).

- width:

  Minimum width of padded strings.

- pattern:

  Pattern to look for.

  The default interpretation is a regular expression, as described in
  [base::regex](https://rdrr.io/r/base/regex.html). Control options with
  `regex()`.

  Match a fixed string (i.e. by comparing only bytes), using `fixed()`.
  This is fast, but approximate.

- replacement:

  A character vector of replacements. Should be either length one, or
  the same length as `string` or `pattern`. References of the form `\1`,
  `\2`, etc. will be replaced with the contents of the respective
  matched group (created by `()`).

  To replace the complete string with `NA`, use
  `replacement = NA_character_`.

  Using a function for `replacement` is not yet supported.

- increment:

  If `TRUE`, increment digits in string by 1. If numeric, increment
  digits in string by value. If `NULL`, 0, or if no digits are present
  in string, return string as is.

- ...:

  Passed to `str_replace_digits()`
