# DE-005: Impossible logical combination

DE-005: Impossible logical combination

## Usage

``` r
rule_de_005_impossible_logical_combination(
  domain,
  var_a = "SEX",
  val_a = "M",
  var_b = "PREGNANT",
  val_b = "Y",
  entity_key_var = "entity_id",
  owner_team = "Data Quality"
)
```

## Arguments

- domain:

  Domain name.

- var_a:

  First variable.

- val_a:

  Disallowed value for `var_a` in combination.

- var_b:

  Second variable.

- val_b:

  Disallowed value for `var_b` in combination.

- entity_key_var:

  Optional entity key variable for output.

- owner_team:

  Owner team.

## Value

A `CoreRule` object.
