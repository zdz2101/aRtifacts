# REL-001: Missing reference records across domains

REL-001: Missing reference records across domains

## Usage

``` r
rule_rel_001_missing_reference_records(
  primary_domain = "PRIMARY",
  reference_domain = "REFERENCE",
  key_vars = c("entity_id"),
  owner_team = "Data Stewardship"
)
```

## Arguments

- primary_domain:

  Domain expected to be fully represented in `reference_domain`.

- reference_domain:

  Domain acting as the reference set.

- key_vars:

  Join keys used for cross-domain reconciliation.

- owner_team:

  Owner team.

## Value

A `CoreRule` object.
