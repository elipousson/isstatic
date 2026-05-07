# Convert a numeric bearing value to the closest cardinal bearing

Convert a numeric bearing value to the closest cardinal bearing

## Usage

``` r
as_cardinal_bearing(x, winds = 8, cols = c("bearing", "cardinal_bearing"))
```

## Arguments

- x:

  A numeric vector with degrees or a data.frame with column name
  matching the first name in cols.

- winds:

  Number of winds to use for results (4, 8, or 16).

- cols:

  A length 2 character vector where the first value is a column name
  containing bearing values and the second is the name of the new column
  added to the data.frame. Required if x is a data.frame.

## Value

A named numeric vector with cardinal bearings (and wind names) or a
data.frame with an added column containing the cardinal bearings.
