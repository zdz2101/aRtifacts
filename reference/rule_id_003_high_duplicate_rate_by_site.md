# ID-003: High duplicate rate by site

ID-003: High duplicate rate by site

## Usage

``` r
rule_id_003_high_duplicate_rate_by_site(
  domain,
  key_vars,
  site_var = "group_id",
  threshold = 0.1,
  entity_key_var = "entity_id",
  owner_team = "Data Stewardship"
)
```

## Arguments

- domain:

  Domain name.

- key_vars:

  Key variables for duplicate detection.

- site_var:

  Grouping variable.

- threshold:

  Duplicate-rate threshold.

- entity_key_var:

  Optional entity key variable for output.

- owner_team:

  Owner team.

## Value

A `CoreRule` object.
