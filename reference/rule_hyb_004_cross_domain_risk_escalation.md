# HYB-004: Cross-domain risk escalation

HYB-004: Cross-domain risk escalation

## Usage

``` r
rule_hyb_004_cross_domain_risk_escalation(
  events_domain = "EVENTS",
  support_domain = "SUPPORT",
  metric_domain = "METRIC",
  group_var = "group_id",
  entity_var = "entity_id",
  metric_var = "metric_value",
  metric_high_cutoff = 100,
  risk_delta_threshold = 0.3,
  owner_team = "Data Operations"
)
```

## Arguments

- events_domain:

  Events domain name.

- support_domain:

  Support domain name.

- metric_domain:

  Numeric metric domain name.

- group_var:

  Group variable.

- entity_var:

  Entity identifier variable.

- metric_var:

  Numeric metric variable.

- metric_high_cutoff:

  High-value threshold in metric domain.

- risk_delta_threshold:

  Absolute risk increase threshold.

- owner_team:

  Owner team.

## Value

A `CoreRule` object.
