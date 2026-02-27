# SNAP-002: Record changed unexpectedly

SNAP-002: Record changed unexpectedly

## Usage

``` r
rule_snap_002_record_changed_unexpectedly(
  domain = "PRIMARY",
  keys = c("entity_id"),
  monitored_fields = character(),
  numeric_tol = 0,
  owner_team = "Standards"
)
```

## Arguments

- domain:

  Domain name.

- keys:

  Identity key columns.

- monitored_fields:

  Fields to compare. If empty, inferred from overlap minus keys.

- numeric_tol:

  Numeric absolute tolerance.

- owner_team:

  Owner team.

## Value

A `CoreRule` object.
