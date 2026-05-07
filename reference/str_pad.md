# Duplicate and concatenate strings within a character vector

Dependency-free drop-in alternative for `stringr::str_pad()`.

## Usage

``` r
str_pad(
  string,
  width,
  side = c("left", "right", "both"),
  pad = " ",
  use_width = TRUE
)
```

## Source

Adapted from the [stringr](https://stringr.tidyverse.org/) package.

## Arguments

- string:

  Input vector. Either a character vector, or something coercible to
  one.

- width:

  Minimum width of padded strings.

- side:

  Side on which padding character is added (left, right or both).

- pad:

  Single padding character (default is a space).

- use_width:

  If `FALSE`, use the length of the string instead of the width; see
  [`str_width()`](https://elipousson.github.io/isstatic/reference/str_width.md)/[`str_length()`](https://elipousson.github.io/isstatic/reference/str_length.md)
  for the difference.

## Value

A character vector.

## Author

Eli Pousson <eli.pousson@gmail.com>
([ORCID](https://orcid.org/0000-0001-8280-1706))

Alexander Rossell Hayes <alexander@rossellhayes.com>
([ORCID](https://orcid.org/0000-0001-9412-0457))
