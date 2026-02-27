# Register A5 cross-snapshot row rules

Register A5 cross-snapshot row rules

## Usage

``` r
register_topology_a5(
  owner_team = "Data Stewardship",
  domain_primary = "PRIMARY",
  keys = c("entity_id"),
  monitored_fields = character(),
  critical_fields = c("status_flag"),
  numeric_tol = 0,
  removed_keys_option = "removed_keys_history",
  identity_stable_fields = c("entity_id", "event_type", "event_date"),
  identity_fields = c("event_id"),
  overwrite = FALSE
)
```

## Arguments

- owner_team:

  Owner team.

- domain_primary:

  Primary domain for snapshot checks.

- keys:

  Identity keys for A5 checks.

- monitored_fields:

  Fields for SNAP-002.

- critical_fields:

  Fields for SNAP-005.

- numeric_tol:

  Numeric tolerance for value changes.

- removed_keys_option:

  Option key for SNAP-003 ledger.

- identity_stable_fields:

  Stable fields used by SNAP-004 to align records.

- identity_fields:

  Fields expected to remain stable after alignment.

- overwrite:

  Whether to overwrite existing IDs.

## Value

Invisibly returns registered IDs.

## Details

Equivalent to `pack_topology_a5(...)` followed by
[`register_rule()`](https://zdz2101.github.io/aRtifacts/reference/register_rule.md)
for each produced rule.
