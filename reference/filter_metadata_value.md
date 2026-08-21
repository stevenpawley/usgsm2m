# Create a metadata filter matching a single value

Metadata filters test dataset-specific fields. Get the available
\`filter_id\` values, and what they mean, from
\`M2MDataset\$filters()\`.

## Usage

``` r
filter_metadata_value(filter_id, value, operand = c("=", "like"))
```

## Arguments

- filter_id:

  The field's filter id, from the \`id\` column of
  \`M2MDataset\$filters()\`.

- value:

  The value to match.

- operand:

  One of "=" or "like".

## Value

A named list describing the filter.

## Examples

``` r
filter_metadata_value("5e83d14fb9436d88", "045")
#> $filterType
#> [1] "value"
#> 
#> $filterId
#> [1] "5e83d14fb9436d88"
#> 
#> $value
#> [1] "045"
#> 
#> $operand
#> [1] "="
#> 
```
