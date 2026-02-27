# GRP-001: Excessive missingness rate

GRP-001: Excessive missingness rate

## Usage

``` r
rule_grp_001_excessive_missingness_rate(
  domain,
  target_vars,
  group_var = "group_id",
  threshold = 0.1,
  owner_team = "Data Stewardship"
)
```

## Arguments

- domain:

  Domain name.

- target_vars:

  Variables to evaluate for missingness.

- group_var:

  Grouping variable.

- threshold:

  Missingness threshold.

- owner_team:

  Owner team.

## Value

A `CoreRule` object.
