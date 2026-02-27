# Register A6 cross-snapshot aggregate rules

Register A6 cross-snapshot aggregate rules

## Usage

``` r
register_topology_a6(
  owner_team = "Data Operations",
  domain_primary = "PRIMARY",
  keys = c("entity_id"),
  group_var = "group_id",
  value_var = "value",
  threshold_row_abs = 100,
  threshold_row_rel = 0.2,
  churn_threshold = 0.1,
  missingness_threshold = 0.1,
  ks_threshold = 0.2,
  site_metric_threshold = 5,
  event_rate_threshold = 0.5,
  overwrite = FALSE
)
```

## Arguments

- owner_team:

  Owner team.

- domain_primary:

  Primary domain for A6 checks.

- keys:

  Identity keys for churn checks.

- group_var:

  Grouping variable.

- value_var:

  Numeric value variable for drift checks.

- threshold_row_abs:

  Threshold for SNAP-006 absolute row change.

- threshold_row_rel:

  Threshold for SNAP-006 relative row change.

- churn_threshold:

  Threshold for SNAP-007 churn rate.

- missingness_threshold:

  Threshold for SNAP-008 missingness increase.

- ks_threshold:

  Threshold for SNAP-009 KS drift.

- site_metric_threshold:

  Threshold for SNAP-010 metric drift.

- event_rate_threshold:

  Threshold for SNAP-011 event-rate change.

- overwrite:

  Whether to overwrite existing IDs.

## Value

Invisibly returns registered IDs.

## Details

Equivalent to `pack_topology_a6(...)` followed by
[`register_rule()`](https://zdz2101.github.io/aRtifacts/reference/register_rule.md)
for each produced rule.
