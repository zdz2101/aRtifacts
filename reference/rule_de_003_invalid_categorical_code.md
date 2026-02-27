# DE-003: Invalid categorical code

DE-003: Invalid categorical code

## Usage

``` r
rule_de_003_invalid_categorical_code(
  domain,
  value_var,
  allowed_levels,
  entity_key_var = "entity_id",
  owner_team = "Data Stewardship"
)
```

## Arguments

- domain:

  Domain name.

- value_var:

  Categorical variable.

- allowed_levels:

  Allowed values.

- entity_key_var:

  Optional entity key variable for output.

- owner_team:

  Owner team.

## Value

A `CoreRule` object.
