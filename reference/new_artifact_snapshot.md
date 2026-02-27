# Create an artifact snapshot

Constructs and validates an `ArtifactSnapshot` that contains one or more
named `DomainSnapshot` objects.

## Usage

``` r
new_artifact_snapshot(
  domains,
  spec_version = NA_character_,
  data_cut = as.character(Sys.Date()),
  build_id = NA_character_,
  code_sha = NA_character_,
  inputs_hash = NA_character_
)
```

## Arguments

- domains:

  Named list of `DomainSnapshot` objects.

- spec_version:

  Specification or standards version label.

- data_cut:

  Data cut identifier/date string.

- build_id:

  Build/run identifier.

- code_sha:

  Source code revision identifier.

- inputs_hash:

  Hash representing upstream inputs.

## Value

A validated `ArtifactSnapshot` object.

## Examples

``` r
dom <- new_domain_snapshot(
  name = "orders",
  data = example_primary(),
  key_vars = c("entity_id", "record_id")
)
art <- new_artifact_snapshot(domains = list(orders = dom))
```
