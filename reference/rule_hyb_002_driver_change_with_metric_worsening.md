# HYB-002: Driver change with metric worsening

HYB-002: Driver change with metric worsening

## Usage

``` r
rule_hyb_002_driver_change_with_metric_worsening(
  driver_domain = "DRIVER",
  metric_domain = "METRIC",
  entity_var = "entity_id",
  driver_var = "driver_value",
  metric_var = "metric_value",
  driver_delta_min = 0,
  metric_delta_min = 0,
  owner_team = "Data Operations"
)
```

## Arguments

- driver_domain:

  Driver domain name.

- metric_domain:

  Metric domain name.

- entity_var:

  Entity identifier variable.

- driver_var:

  Numeric driver variable.

- metric_var:

  Numeric metric variable.

- driver_delta_min:

  Minimum driver increase.

- metric_delta_min:

  Minimum metric worsening increase.

- owner_team:

  Owner team.

## Value

A `CoreRule` object.
