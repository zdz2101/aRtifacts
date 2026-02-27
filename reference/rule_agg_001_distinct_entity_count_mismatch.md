# AGG-001: Distinct entity count mismatch

AGG-001: Distinct entity count mismatch

## Usage

``` r
rule_agg_001_distinct_entity_count_mismatch(
  left_domain = "LEFT",
  right_domain = "RIGHT",
  entity_var = "entity_id",
  tolerance = 0,
  owner_team = "Data Stewardship"
)
```

## Arguments

- left_domain:

  Left domain name.

- right_domain:

  Right domain name.

- entity_var:

  Entity identifier variable.

- tolerance:

  Allowed absolute difference in counts.

- owner_team:

  Owner team.

## Value

A `CoreRule` object.
