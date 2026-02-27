# TMP-001: Invalid date format

TMP-001: Invalid date format

## Usage

``` r
rule_tmp_001_invalid_date_format(
  domain,
  date_var,
  entity_key_var = "entity_id",
  owner_team = "Data Stewardship"
)
```

## Arguments

- domain:

  Domain name.

- date_var:

  Date variable expected in YYYY-MM-DD format.

- entity_key_var:

  Optional entity key variable for output.

- owner_team:

  Owner team.

## Value

A `CoreRule` object.
