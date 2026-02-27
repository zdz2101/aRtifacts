# Artifact snapshot S7 class

Represents a study-level snapshot made of multiple validated
`DomainSnapshot` objects and shared metadata.

## Usage

``` r
ArtifactSnapshot(
  domains = list(),
  spec_version = character(0),
  data_cut = character(0),
  build_id = character(0),
  code_sha = character(0),
  inputs_hash = character(0)
)
```

## Arguments

- domains:

  Named list of `DomainSnapshot` objects.

- spec_version:

  Optional specification or standards version label.

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
