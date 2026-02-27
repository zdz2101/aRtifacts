# SNAP-003: Record reappeared

Flags keys in current data that are present in historical removed-key
ledger.

## Usage

``` r
rule_snap_003_record_reappeared(
  domain = "PRIMARY",
  keys = c("entity_id"),
  removed_keys_option = "removed_keys_history",
  owner_team = "Data Operations"
)
```

## Arguments

- domain:

  Domain name.

- keys:

  Identity key columns.

- removed_keys_option:

  Context option name carrying removed keys.

- owner_team:

  Owner team.

## Value

A `CoreRule` object.
