# Domain snapshot S7 class

Represents one keyed dataset ("domain") at a specific point in time,
with validation on key variables and key uniqueness. Schema-agnostic —
works with any tabular data, not limited to SDTM or ADaM.

## Usage

``` r
DomainSnapshot(
  name = character(0),
  data = NULL,
  key_vars = character(0),
  spec_version = character(0),
  data_cut = character(0),
  build_id = character(0),
  code_sha = character(0),
  inputs_hash = character(0)
)
```

## Arguments

- name:

  Domain or table name (e.g. `"orders"`, `"orders"`).

- data:

  A `data.frame` containing domain rows.

- key_vars:

  Character vector of columns that uniquely identify rows.

- spec_version:

  Optional specification or schema version label.

- data_cut:

  Data cut identifier/date string.

- build_id:

  Build/run identifier.

- code_sha:

  Source code revision identifier.

- inputs_hash:

  Hash representing upstream inputs.

## Value

An S7 class object.
