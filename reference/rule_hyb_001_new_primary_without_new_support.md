# HYB-001: New primary records without new support

HYB-001: New primary records without new support

## Usage

``` r
rule_hyb_001_new_primary_without_new_support(
  primary_domain = "PRIMARY",
  support_domain = "SUPPORT",
  primary_keys = c("entity_id", "record_id"),
  match_keys = c("entity_id"),
  owner_team = "Data Operations"
)
```

## Arguments

- primary_domain:

  Primary domain tracked across snapshots.

- support_domain:

  Supporting domain expected to co-evolve.

- primary_keys:

  Keys defining a primary record.

- match_keys:

  Keys used to match primary to support records.

- owner_team:

  Owner team.

## Value

A `CoreRule` object.
