# SNAP-011: Event rate change anomaly

SNAP-011: Event rate change anomaly

## Usage

``` r
rule_snap_011_event_rate_change_anomaly(
  domain = "PRIMARY",
  group_var = "group_id",
  subject_var = "entity_id",
  threshold_abs_change = 0.5,
  owner_team = "Data Operations"
)
```

## Arguments

- domain:

  Domain name.

- group_var:

  Grouping variable.

- subject_var:

  Subject identifier variable.

- threshold_abs_change:

  Absolute threshold for event-rate change.

- owner_team:

  Owner team.

## Value

A `CoreRule` object.
