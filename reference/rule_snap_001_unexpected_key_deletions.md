# SNAP-001: Unexpected key deletions

SNAP-001: Unexpected key deletions

## Usage

``` r
rule_snap_001_unexpected_key_deletions(
  domain = "PRIMARY",
  keys = c("entity_id"),
  owner_team = "Standards"
)
```

## Arguments

- domain:

  Domain name.

- keys:

  Identity key columns.

- owner_team:

  Owner team.

## Value

A `CoreRule` object.
