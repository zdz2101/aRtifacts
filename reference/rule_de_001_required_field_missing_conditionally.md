# DE-001: Required field missing conditionally

DE-001: Required field missing conditionally

## Usage

``` r
rule_de_001_required_field_missing_conditionally(
  domain,
  condition_var,
  required_var,
  condition_value = "Y",
  entity_key_var = "entity_id",
  owner_team = "Data Stewardship"
)
```

## Arguments

- domain:

  Domain name.

- condition_var:

  Condition variable.

- required_var:

  Required variable when condition is met.

- condition_value:

  Value in `condition_var` that triggers requirement.

- entity_key_var:

  Optional entity key variable for output.

- owner_team:

  Owner team.

## Value

A `CoreRule` object.
