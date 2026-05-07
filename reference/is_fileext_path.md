# Does this text end in the provided file extension?

Does this text end in the provided file extension?

is_geojson_fileext: Does this text end with a GeoJSON file extension?

is_csv_fileext: Does this text end with a CSV file extension?

is_excel_fileext: Does this text end with a XLS or XLSX file extension?

is_rdata_fileext: Does this text end with a rds, rda, or RData file
extension?

is_rds_fileext: Does this text end with a rds file extension?

is_rda_fileext: Does this text end with a rda file extension?

is_zip_fileext: Does this text end with a zip file extension?

## Usage

``` r
is_fileext_path(x, fileext, ignore.case = TRUE)

is_geojson_fileext(x, ignore.case = TRUE)

is_csv_fileext(x, ignore.case = TRUE)

is_excel_fileext(x, ignore.case = TRUE)

is_rdata_fileext(x, ignore.case = TRUE)

is_rds_fileext(x, ignore.case = TRUE)

is_rda_fileext(x, ignore.case = TRUE)

is_zip_fileext(x, ignore.case = TRUE)
```

## Arguments

- x:

  A character vector to check for matches, or an object which can be
  coerced by [`as.character()`](https://rdrr.io/r/base/character.html)
  to a character vector.

- fileext:

  A file extension to compare to x. Required. If a vector of multiple
  extensions are provided, returns `TRUE` for any match.

- ignore.case:

  logical. if `FALSE`, the pattern matching is *case sensitive* and if
  `TRUE`, case is ignored during matching.

## See also

[`has_fileext()`](https://elipousson.github.io/isstatic/reference/has_fileext.md)
