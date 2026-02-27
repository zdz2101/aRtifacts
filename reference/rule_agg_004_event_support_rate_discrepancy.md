# AGG-004: Event-support rate discrepancy

AGG-004: Event-support rate discrepancy

## Usage

``` r
rule_agg_004_event_support_rate_discrepancy(
  event_domain = "EVENTS",
  support_domain = "SUPPORT",
  group_var = "group_id",
  entity_var = "entity_id",
  event_high_threshold = 0.5,
  support_low_threshold = 0.5,
  owner_team = "Data Operations"
)
```

## Arguments

- event_domain:

  Event domain name.

- support_domain:

  Support domain name.

- group_var:

  Group variable.

- entity_var:

  Entity identifier variable.

- event_high_threshold:

  High-threshold for events per entity.

- support_low_threshold:

  Low-threshold for support coverage.

- owner_team:

  Owner team.

## Value

A `CoreRule` object.
