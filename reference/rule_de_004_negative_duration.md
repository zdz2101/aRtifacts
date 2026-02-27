# DE-004: Negative duration

DE-004: Negative duration

## Usage

``` r
rule_de_004_negative_duration(
  domain,
  duration_var,
  entity_key_var = "entity_id",
  owner_team = "Data Quality"
)
```

## Arguments

- domain:

  Domain name.

- duration_var:

  Duration variable.

- entity_key_var:

  Optional entity key variable for output.

- owner_team:

  Owner team.

## Value

A `CoreRule` object.
