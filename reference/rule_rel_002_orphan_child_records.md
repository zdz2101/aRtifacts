# REL-002: Orphan child records

REL-002: Orphan child records

## Usage

``` r
rule_rel_002_orphan_child_records(
  child_domain = "CHILD",
  parent_domain = "PARENT",
  parent_keys = c("entity_id"),
  owner_team = "Data Stewardship"
)
```

## Arguments

- child_domain:

  Child domain name.

- parent_domain:

  Parent domain name.

- parent_keys:

  Parent keys expected in both domains.

- owner_team:

  Owner team.

## Value

A `CoreRule` object.
