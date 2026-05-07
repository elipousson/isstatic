# Set start number for numeric vector x

Helper for
[`as_numbered_labels()`](https://elipousson.github.io/isstatic/reference/as_numbered_labels.md).

## Usage

``` r
set_start_number(x, start = NULL, labels = "arabic")
```

## Arguments

- x:

  An integer or other vector or a data.frame. An integer vector or
  integer column is used as the number that is converted based on the
  label style. If x is not an integer or data.frame with an integer
  column, the numbering is created based on
  [`seq_along()`](https://rdrr.io/r/base/seq.html).

- start:

  Starting number or value. Letters are supported if label style is
  "alph", "Alph", "alpha", or "Alpha" and Roman numerals are supported
  if label is "roman" or "Roman".

- labels:

  Label style. Options include "arabic", "alph", "Alph", "alpha",
  "Alpha", "roman", or "Roman".
