# What is the orientation of a numeric aspect ratio?

What is the orientation of a numeric aspect ratio?

## Usage

``` r
as_orientation(x, tolerance = 0.1, cols = c("width", "height"))
```

## Arguments

- x:

  A numeric vector with an aspect ratio or a data.frame with width and
  height column (using width and height values from columns matching the
  cols parameter).

- tolerance:

  Positive numeric value above or below 1 used to determine if an aspect
  ratio is square, landscape, or portrait.

- cols:

  Name of width and height column if x is a data.frame object.

## Value

A character vector of orientations of the same length as x or, if x is a
data.frame, the same length as the number of rows in x.
