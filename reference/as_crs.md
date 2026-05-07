# Coerce a sf, sfc, or bbox object to a coordinate reference system

This function should be updated to support stars and terra objects.

## Usage

``` r
as_crs(x, input = TRUE)

is_lonlat_crs(x, crs = c("EPSG:4326", "EPSG:4269"))
```

## Arguments

- x:

  A sf, sfc, or bbox object to coerce into a CRS.

- input:

  If `TRUE` (default), return only the "input" component of the crs
  object. If `FALSE`, return the full crs object.

- crs:

  For is_lonlat_crs, coordinate reference system to use as lonlat crs.
