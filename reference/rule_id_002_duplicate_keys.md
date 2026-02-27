# ID-002: Duplicate keys

ID-002: Duplicate keys

## Usage

``` r
rule_id_002_duplicate_keys(
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

  Key variables expected to uniquely identify records.

- entity_key_var:

  Optional entity key variable for output.

- owner_team:

  Owner team.

## Value

A `CoreRule` object.
