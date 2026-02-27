# Build A4 cross-domain aggregate starter pack

Build A4 cross-domain aggregate starter pack

## Usage

``` r
pack_topology_a4(
  owner_team = "Data Operations",
  left_domain = "LEFT",
  right_domain = "RIGHT",
  entity_var = "entity_id",
  group_var = "group_id",
  entity_count_tolerance = 0,
  group_count_tolerance = 0,
  completeness_domains = c("PRIMARY", "SUPPORT", "AUX"),
  completeness_gap_threshold = 0.2,
  event_domain = "EVENTS",
  support_domain = "SUPPORT",
  event_high_threshold = 0.5,
  support_low_threshold = 0.5
)
```

## Arguments

- owner_team:

  Owner team.

- left_domain:

  Left domain name.

- right_domain:

  Right domain name.

- entity_var:

  Entity variable for count reconciliation.

- group_var:

  Group variable for grouped checks.

- entity_count_tolerance:

  Tolerance for AGG-001.

- group_count_tolerance:

  Tolerance for AGG-002.

- completeness_domains:

  Domains for AGG-003.

- completeness_gap_threshold:

  Gap threshold for AGG-003.

- event_domain:

  Event domain for AGG-004.

- support_domain:

  Support domain for AGG-004.

- event_high_threshold:

  Event-rate threshold for AGG-004.

- support_low_threshold:

  Support-rate threshold for AGG-004.

## Value

List of `CoreRule` objects.
