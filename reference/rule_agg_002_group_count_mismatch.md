# AGG-002: Group count mismatch across domains

AGG-002: Group count mismatch across domains

## Usage

``` r
rule_agg_002_group_count_mismatch(
  left_domain = "LEFT",
  right_domain = "RIGHT",
  group_var = "group_id",
  tolerance = 0,
  owner_team = "Data Stewardship"
)
```

## Arguments

- left_domain:

  Left domain name.

- right_domain:

  Right domain name.

- group_var:

  Group variable present in both domains.

- tolerance:

  Allowed absolute difference per group.

- owner_team:

  Owner team.

## Value

A `CoreRule` object.
