# HYB-003: Entity removed from primary but present elsewhere

HYB-003: Entity removed from primary but present elsewhere

## Usage

``` r
rule_hyb_003_entity_removed_but_present_elsewhere(
  primary_domain = "PRIMARY",
  other_domain = "OTHER",
  entity_var = "entity_id",
  owner_team = "Data Stewardship"
)
```

## Arguments

- primary_domain:

  Primary domain name.

- other_domain:

  Other domain checked in current snapshot.

- entity_var:

  Entity identifier variable.

- owner_team:

  Owner team.

## Value

A `CoreRule` object.
