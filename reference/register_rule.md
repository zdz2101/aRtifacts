# Register a core rule

Registers a `CoreRule` for lookup and pack composition.

## Usage

``` r
register_rule(rule, overwrite = FALSE)
```

## Arguments

- rule:

  A `CoreRule`.

- overwrite:

  Whether to overwrite an existing rule with the same ID.

## Value

Invisibly returns the registered rule ID.
