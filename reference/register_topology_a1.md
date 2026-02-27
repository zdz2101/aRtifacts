# Register A1 intra-domain row rules

Register A1 intra-domain row rules

## Usage

``` r
register_topology_a1(
  domain,
  key_vars,
  date_var = NULL,
  start_var = NULL,
  end_var = NULL,
  time_var = NULL,
  condition_var = NULL,
  condition_value = "Y",
  required_var = NULL,
  numeric_var = NULL,
  numeric_min = -Inf,
  numeric_max = Inf,
  categorical_var = NULL,
  allowed_levels = character(),
  duration_var = NULL,
  logic_var_a = "state_a",
  logic_val_a = "M",
  logic_var_b = "state_b",
  logic_val_b = "Y",
  entity_key_var = "entity_id",
  owner_team = "Data Stewardship",
  overwrite = FALSE
)
```

## Arguments

- domain:

  Domain name.

- key_vars:

  Key variables for ID-001.

- date_var:

  Date variable for TMP-001.

- start_var:

  Start date variable for TMP-002/TMP-003.

- end_var:

  End date variable for TMP-002/TMP-003.

- time_var:

  Time variable for TMP-004.

- condition_var:

  Condition variable for DE-001.

- condition_value:

  Trigger value for DE-001.

- required_var:

  Required variable for DE-001.

- numeric_var:

  Numeric variable for DE-002.

- numeric_min:

  Lower bound for DE-002.

- numeric_max:

  Upper bound for DE-002.

- categorical_var:

  Categorical variable for DE-003.

- allowed_levels:

  Allowed levels for DE-003.

- duration_var:

  Duration variable for DE-004.

- logic_var_a:

  First variable for DE-005.

- logic_val_a:

  Disallowed value for first variable.

- logic_var_b:

  Second variable for DE-005.

- logic_val_b:

  Disallowed value for second variable.

- entity_key_var:

  Optional entity key variable in outputs.

- owner_team:

  Owner team.

- overwrite:

  Whether to overwrite existing IDs.

## Value

Invisibly returns registered IDs.

## Details

Equivalent to `pack_topology_a1(...)` followed by
[`register_rule()`](https://zdz2101.github.io/aRtifacts/reference/register_rule.md)
for each produced rule.
