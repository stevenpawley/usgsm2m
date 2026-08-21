# Create a metadata filter matching a range of values

Create a metadata filter matching a range of values

## Usage

``` r
filter_metadata_between(filter_id, first, second)
```

## Arguments

- filter_id:

  The field's filter id, from the \`id\` column of
  \`M2MDataset\$filters()\`.

- first:

  The lower bound.

- second:

  The upper bound.

## Value

A named list describing the filter.

## Examples

``` r
filter_metadata_between("5e83d14fb9436d88", 40, 45)
#> $filterType
#> [1] "between"
#> 
#> $filterId
#> [1] "5e83d14fb9436d88"
#> 
#> $firstValue
#> [1] 40
#> 
#> $secondValue
#> [1] 45
#> 
```
