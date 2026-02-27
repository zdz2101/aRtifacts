# ID-001: Missing key fields

ID-001: Missing key fields

## Usage

``` r
rule_id_001_missing_key_fields(
  domain,
  key_vars,
  entity_key_var = "entity_id",
  owner_team = "Data Stewardship"
)
```

## Arguments

- domain:

  Domain name.

- key_vars:

  Key variables expected to be non-missing/non-blank.

- entity_key_var:

  Optional entity key variable for output.

- owner_team:

  Owner team.

## Value

A `CoreRule` object.
