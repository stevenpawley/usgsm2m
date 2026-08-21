# Create a spatial filter for use in a scene search

Create a spatial filter for use in a scene search

## Usage

``` r
filter_spatial(ll_lon, ll_lat, ur_lon, ur_lat)
```

## Arguments

- ll_lon:

  Lower left longitude

- ll_lat:

  Lower left latitude

- ur_lon:

  Upper right longitude

- ur_lat:

  Upper right latitude

## Value

A named list with spatial filter parameters

## Examples

``` r
filter_spatial(ll_lon = -120, ll_lat = 40, ur_lon = -119, ur_lat = 41)
#> $filterType
#> [1] "mbr"
#> 
#> $lowerLeft
#> $lowerLeft$latitude
#> [1] 40
#> 
#> $lowerLeft$longitude
#> [1] -120
#> 
#> 
#> $upperRight
#> $upperRight$latitude
#> [1] 41
#> 
#> $upperRight$longitude
#> [1] -119
#> 
#> 
```
