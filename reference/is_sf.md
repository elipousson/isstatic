# Is this a sf class object?

Is this a sf class object?

Is this a sfc class object?

Is this a sfg class object?

Is this a bbox class object?

Is this a sf, sfc, or bbox class object?

Is this a RasterLayer class object?

Is this a Spatial class (sp) object?

## Usage

``` r
is_sf(x)

is_sfc(x)

is_sfg(x)

is_bbox(x)

is_sf_ext(x, ext = TRUE)

is_raster(x)

is_sp(x)
```

## Arguments

- x:

  An object to be tested with
  [`inherits()`](https://rdrr.io/r/base/class.html)

- ext:

  If `TRUE`, return `TRUE` is x is a sf, sfc, or bbox object. If
  `FALSE`, only check if x is an sf object. If ext is a character
  object, it is passed to the what parameter of
  [`inherits()`](https://rdrr.io/r/base/class.html) with sf.

## See also

[`as_crs()`](https://elipousson.github.io/isstatic/reference/as_crs.md)
