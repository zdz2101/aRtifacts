# Register A3 cross-domain relational starter rules

Register A3 cross-domain relational starter rules

## Usage

``` r
register_topology_a3(
  owner_team = "Data Stewardship",
  primary_domain = "PRIMARY",
  reference_domain = "REFERENCE",
  child_domain = "CHILD",
  parent_domain = "PARENT",
  parent_keys = c("entity_id"),
  left_domain = "LEFT",
  right_domain = "RIGHT",
  entity_var = "entity_id",
  left_group_var = "group_id",
  right_group_var = "group_id",
  event_domain = "EVENTS",
  window_domain = "WINDOW",
  link_keys = c("entity_id"),
  event_time_var = "event_time",
  window_start_var = "window_start",
  window_end_var = "window_end",
  window_days = 0,
  overwrite = FALSE
)
```

## Arguments

- owner_team:

  Owner team.

- primary_domain:

  Domain expected to reference `reference_domain`.

- reference_domain:

  Domain used as relational reference.

- child_domain:

  Child domain for orphan detection.

- parent_domain:

  Parent domain for orphan detection.

- parent_keys:

  Parent key columns.

- left_domain:

  Left domain for pair mismatch checks.

- right_domain:

  Right domain for pair mismatch checks.

- entity_var:

  Entity variable for pair mismatch checks.

- left_group_var:

  Group variable in `left_domain`.

- right_group_var:

  Group variable in `right_domain`.

- event_domain:

  Event domain for window checks.

- window_domain:

  Window domain for window checks.

- link_keys:

  Link keys for event/window alignment.

- event_time_var:

  Event timestamp variable.

- window_start_var:

  Window start variable.

- window_end_var:

  Window end variable.

- window_days:

  Allowed window extension in days.

- overwrite:

  Whether to overwrite existing IDs.

## Value

Invisibly returns registered IDs.
