# SNAP-004: Identity drift

SNAP-004: Identity drift

## Usage

``` r
rule_snap_004_identity_drift(
  domain = "PRIMARY",
  stable_fields = c("entity_id", "event_type", "event_date"),
  identity_fields = c("event_id"),
  owner_team = "Standards"
)
```

## Arguments

- domain:

  Domain name.

- stable_fields:

  Fields defining a stable event identity.

- identity_fields:

  Fields expected to remain stable for each event.

- owner_team:

  Owner team.

## Value

A `CoreRule` object.
