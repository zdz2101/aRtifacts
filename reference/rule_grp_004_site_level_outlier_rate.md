# GRP-004: Group-level outlier rate

GRP-004: Group-level outlier rate

## Usage

``` r
rule_grp_004_site_level_outlier_rate(
  domain,
  site_var = "group_id",
  event_flag_var = NULL,
  k = 3,
  owner_team = "Data Operations"
)
```

## Arguments

- domain:

  Domain name.

- site_var:

  Grouping variable.

- event_flag_var:

  Optional binary event variable (`1`/`0`, `Y`/`N`).

- k:

  Robust z-score threshold.

- owner_team:

  Owner team.

## Value

A `CoreRule` object.
