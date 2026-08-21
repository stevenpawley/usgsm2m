# Combine metadata filters with a logical operator

Combine metadata filters with a logical operator

## Usage

``` r
filter_metadata_and(...)

filter_metadata_or(...)
```

## Arguments

- ...:

  Metadata filters to combine, as built by \[filter_metadata_value()\]
  or \[filter_metadata_between()\].

## Value

A named list describing the combined filter.

## Examples

``` r
filter_metadata_and(
  filter_metadata_value("5e83d14fb9436d88", "045"),
  filter_metadata_value("5e83d14ff1eda1b8", "027")
)
#> $filterType
#> [1] "and"
#> 
#> $childFilters
#> $childFilters[[1]]
#> $childFilters[[1]]$filterType
#> [1] "value"
#> 
#> $childFilters[[1]]$filterId
#> [1] "5e83d14fb9436d88"
#> 
#> $childFilters[[1]]$value
#> [1] "045"
#> 
#> $childFilters[[1]]$operand
#> [1] "="
#> 
#> 
#> $childFilters[[2]]
#> $childFilters[[2]]$filterType
#> [1] "value"
#> 
#> $childFilters[[2]]$filterId
#> [1] "5e83d14ff1eda1b8"
#> 
#> $childFilters[[2]]$value
#> [1] "027"
#> 
#> $childFilters[[2]]$operand
#> [1] "="
#> 
#> 
#> 
```
