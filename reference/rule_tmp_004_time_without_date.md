# TMP-004: Time without date

TMP-004: Time without date

## Usage

``` r
rule_tmp_004_time_without_date(
  domain,
  time_var,
  date_var,
  entity_key_var = "entity_id",
  owner_team = "Data Stewardship"
)
```

## Arguments

- domain:

  Domain name.

- time_var:

  Time variable.

- date_var:

  Date variable.

- entity_key_var:

  Optional entity key variable for output.

- owner_team:

  Owner team.

## Value

A `CoreRule` object.
