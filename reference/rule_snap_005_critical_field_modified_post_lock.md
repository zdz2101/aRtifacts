# SNAP-005: Critical field modified post-lock

SNAP-005: Critical field modified post-lock

## Usage

``` r
rule_snap_005_critical_field_modified_post_lock(
  domain = "PRIMARY",
  keys = c("entity_id"),
  critical_fields = c("status_flag"),
  numeric_tol = 0,
  owner_team = "Standards"
)
```

## Arguments

- domain:

  Domain name.

- keys:

  Identity key columns.

- critical_fields:

  Fields that must not change post-lock.

- numeric_tol:

  Numeric absolute tolerance.

- owner_team:

  Owner team.

## Value

A `CoreRule` object.
