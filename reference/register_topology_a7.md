# Register A7 cross-domain + cross-snapshot starter rules

Register A7 cross-domain + cross-snapshot starter rules

## Usage

``` r
register_topology_a7(
  owner_team = "Data Operations",
  primary_domain = "PRIMARY",
  support_domain = "SUPPORT",
  primary_keys = c("entity_id", "record_id"),
  match_keys = c("entity_id"),
  other_domain = "OTHER",
  driver_domain = "DRIVER",
  metric_domain = "METRIC",
  entity_var = "entity_id",
  driver_var = "driver_value",
  metric_var = "metric_value",
  driver_delta_min = 0,
  metric_delta_min = 0,
  events_domain = "EVENTS",
  group_var = "group_id",
  metric_high_cutoff = 100,
  risk_delta_threshold = 0.3,
  overwrite = FALSE
)
```

## Arguments

- owner_team:

  Owner team.

- primary_domain:

  Primary domain for HYB-001/HYB-003.

- support_domain:

  Support domain for HYB-001/HYB-004.

- primary_keys:

  Identity keys for HYB-001.

- match_keys:

  Match keys for HYB-001.

- other_domain:

  Other domain for HYB-003.

- driver_domain:

  Driver domain for HYB-002.

- metric_domain:

  Metric domain for HYB-002/HYB-004.

- entity_var:

  Entity variable.

- driver_var:

  Driver variable.

- metric_var:

  Metric variable.

- driver_delta_min:

  Driver threshold for HYB-002.

- metric_delta_min:

  Metric threshold for HYB-002.

- events_domain:

  Events domain for HYB-004.

- group_var:

  Group variable for HYB-004.

- metric_high_cutoff:

  High metric threshold for HYB-004.

- risk_delta_threshold:

  Risk delta threshold for HYB-004.

- overwrite:

  Whether to overwrite existing IDs.

## Value

Invisibly returns registered IDs.
