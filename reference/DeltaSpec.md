# Delta spec S7 class

Defines field-level comparison and tolerance for snapshot deltas.

## Usage

``` r
DeltaSpec(
  fields = character(0),
  abs_tol = integer(0),
  rel_tol = integer(0),
  method = character(0)
)
```

## Arguments

- fields:

  Character vector of fields to compare.

- abs_tol:

  Absolute tolerance for numeric comparisons.

- rel_tol:

  Relative tolerance for numeric comparisons.

- method:

  Comparison method (`"exact"`, `"tolerant"`).

## Value

An S7 class object.
