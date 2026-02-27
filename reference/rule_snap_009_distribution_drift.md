# SNAP-009: Distribution drift

SNAP-009: Distribution drift

## Usage

``` r
rule_snap_009_distribution_drift(
  domain = "PRIMARY",
  value_var = "value",
  ks_threshold = 0.2,
  owner_team = "Data Operations"
)
```

## Arguments

- domain:

  Domain name.

- value_var:

  Numeric variable for distribution comparison.

- ks_threshold:

  KS-statistic threshold.

- owner_team:

  Owner team.

## Value

A `CoreRule` object.
