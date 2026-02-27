# SNAP-007: Key churn spike

SNAP-007: Key churn spike

## Usage

``` r
rule_snap_007_key_churn_spike(
  domain = "PRIMARY",
  keys = c("entity_id"),
  churn_threshold = 0.1,
  owner_team = "Data Operations"
)
```

## Arguments

- domain:

  Domain name.

- keys:

  Identity key columns.

- churn_threshold:

  Maximum expected churn rate.

- owner_team:

  Owner team.

## Value

A `CoreRule` object.
