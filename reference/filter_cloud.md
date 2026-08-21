# Create a cloud cover filter for use in a scene search

Create a cloud cover filter for use in a scene search

## Usage

``` r
filter_cloud(min = 0, max = 100)
```

## Arguments

- min:

  Minimum cloud cover percentage (0-100)

- max:

  Maximum cloud cover percentage (0-100)

## Value

A named list with min and max cloud cover percentages

## Examples

``` r
filter_cloud(min = 0, max = 30)
#> $min
#> [1] 0
#> 
#> $max
#> [1] 30
#> 
```
