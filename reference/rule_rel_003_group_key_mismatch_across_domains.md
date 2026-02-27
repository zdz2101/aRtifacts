# REL-003: Group-key mismatch across domains

REL-003: Group-key mismatch across domains

## Usage

``` r
rule_rel_003_group_key_mismatch_across_domains(
  left_domain = "LEFT",
  right_domain = "RIGHT",
  entity_var = "entity_id",
  left_group_var = "group_id",
  right_group_var = "group_id",
  owner_team = "Data Stewardship"
)
```

## Arguments

- left_domain:

  Left domain name.

- right_domain:

  Right domain name.

- entity_var:

  Entity identifier variable.

- left_group_var:

  Group variable in left domain.

- right_group_var:

  Group variable in right domain.

- owner_team:

  Owner team.

## Value

A `CoreRule` object.
