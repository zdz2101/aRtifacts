# DE-002: Out-of-range numeric values

DE-002: Out-of-range numeric values

## Usage

``` r
rule_de_002_out_of_range_numeric_values(
  domain,
  value_var,
  lower,
  upper,
  entity_key_var = "entity_id",
  owner_team = "Data Quality"
)
```

## Arguments

- domain:

  Domain name.

- value_var:

  Numeric variable to evaluate.

- lower:

  Lower bound (inclusive).

- upper:

  Upper bound (inclusive).

- entity_key_var:

  Optional entity key variable for output.

- owner_team:

  Owner team.

## Value

A `CoreRule` object.
