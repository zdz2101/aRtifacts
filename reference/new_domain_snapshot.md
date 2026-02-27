# Create a domain snapshot

Constructs and validates a `DomainSnapshot`.

## Usage

``` r
new_domain_snapshot(
  name,
  data,
  key_vars,
  spec_version = NA_character_,
  data_cut = as.character(Sys.Date()),
  build_id = NA_character_,
  code_sha = NA_character_,
  inputs_hash = NA_character_
)
```

## Arguments

- name:

  Domain or table name (for example, `"orders"`, `"orders"`,
  `"lab_results"`).

- data:

  A `data.frame` containing domain rows.

- key_vars:

  Character vector of key columns that uniquely identify rows.

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

A validated `DomainSnapshot` object.

## Examples

``` r
dom <- new_domain_snapshot(
  name = "orders",
  data = example_primary(),
  key_vars = c("entity_id", "record_id")
)
```
