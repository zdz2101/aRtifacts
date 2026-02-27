# Rule context S7 class

Execution context used by topology-dispatched rules.

## Usage

``` r
RuleContext(current = NULL, previous = NULL, options = list())
```

## Arguments

- current:

  Current `ArtifactSnapshot`.

- previous:

  Optional previous `ArtifactSnapshot`.

- options:

  Named list of runtime options (thresholds, tolerances, etc.).

## Value

An S7 class object.
