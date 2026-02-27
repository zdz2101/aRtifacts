# SNAP-010: Site metric drift

SNAP-010: Site metric drift

## Usage

``` r
rule_snap_010_site_metric_drift(
  domain = "PRIMARY",
  group_var = "group_id",
  value_var = "value",
  threshold_abs_diff = 5,
  owner_team = "Data Operations"
)
```

## Arguments

- domain:

  Domain name.

- group_var:

  Grouping variable.

- value_var:

  Numeric metric variable.

- threshold_abs_diff:

  Absolute threshold for metric change.

- owner_team:

  Owner team.

## Value

A `CoreRule` object.
