# SNAP-008: Missingness rate drift

SNAP-008: Missingness rate drift

## Usage

``` r
rule_snap_008_missingness_rate_drift(
  domain = "PRIMARY",
  group_var = "group_id",
  value_var = "value",
  threshold_increase = 0.1,
  owner_team = "Data Operations"
)
```

## Arguments

- domain:

  Domain name.

- group_var:

  Grouping variable.

- value_var:

  Variable to assess missingness drift.

- threshold_increase:

  Threshold for missingness increase.

- owner_team:

  Owner team.

## Value

A `CoreRule` object.
