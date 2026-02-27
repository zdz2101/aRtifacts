# GRP-005: Distribution skew anomaly

GRP-005: Distribution skew anomaly

## Usage

``` r
rule_grp_005_distribution_skew_anomaly(
  domain,
  value_var,
  threshold = 2,
  owner_team = "Biostats"
)
```

## Arguments

- domain:

  Domain name.

- value_var:

  Numeric variable.

- threshold:

  Absolute skewness threshold.

- owner_team:

  Owner team.

## Value

A `CoreRule` object.
