# Create a temporal filter for use in a scene search

Create a temporal filter for use in a scene search

## Usage

``` r
filter_temporal(start, end)
```

## Arguments

- start:

  Start date in "YYYY-MM-DD" format

- end:

  End date in "YYYY-MM-DD" format

## Value

A named list with start and end dates

## Examples

``` r
filter_temporal("2020-07-01", "2020-07-31")
#> $start
#> [1] "2020-07-01"
#> 
#> $end
#> [1] "2020-07-31"
#> 
```
