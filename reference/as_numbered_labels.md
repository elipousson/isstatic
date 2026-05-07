# Convert a numeric vector to a vector of numbered labels

This function allows the creation of numbered labels for a vector using
a range of numbering styles.

## Usage

``` r
as_numbered_labels(
  x,
  labels = "arabic",
  start = NULL,
  suffix = NULL,
  base = 26,
  cols = "num_label",
  pad = NULL,
  side = "left",
  quiet = TRUE,
  call = parent.frame()
)
```

## Arguments

- x:

  An integer or other vector or a data.frame. An integer vector or
  integer column is used as the number that is converted based on the
  label style. If x is not an integer or data.frame with an integer
  column, the numbering is created based on
  [`seq_along()`](https://rdrr.io/r/base/seq.html).

- labels:

  Label style. Options include "arabic", "alph", "Alph", "alpha",
  "Alpha", "roman", or "Roman".

- start:

  Starting number or value. Letters are supported if label style is
  "alph", "Alph", "alpha", or "Alpha" and Roman numerals are supported
  if label is "roman" or "Roman".

- suffix:

  Suffix character to follow number labels. For example, if `x = 1` and
  `suffix = "."` the returned label would be "1."

- base:

  Base used in alphabetical number labels. Highest letter to use for
  alphabetical numbers. Single digit letters (A to Z) or numbers 1 to 26
  are supported. For example, if base is 3, alphabetical labels for
  numbers higher than 3 have the prior value prefixed so 3 would be "C"
  and 4 would be "AA". Defaults to 26 which converts 27 to "AA", 53 to
  "BA", etc.

- cols:

  Column name to use for added column for number labels when x is a
  data.frame. Defaults to "num_label". If cols is length 2, the first
  item in the vector is assumed to be the column name from the
  data.frame to use as x and the second item is used as the column name
  for the added column with number labels.

- pad, side:

  If pad is not `NULL`, pass pad and side to
  [`str_pad()`](https://elipousson.github.io/isstatic/reference/str_pad.md)
  added from the `stringstatic::str_pad()` package.

- quiet:

  If `TRUE`, suppress warnings about creation of NA values through
  coercion of object types. Default to `TRUE`.

- call:

  Default: [`parent.frame()`](https://rdrr.io/r/base/sys.parent.html).
  Passed to input checking functions to improve error message traceback.

## Value

- If x is a vector, function returns numeric vector if labels is
  "arabic" or a character vector otherwise.

- If x is a data.frame, `as_numbered_labels()` returns a modified
  data.frame with an added column with a name matching the second value
  of the cols parameter.
