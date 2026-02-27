# Register A0 schema rules

Registers the schema pack into the rule registry.

## Usage

``` r
register_topology_a0(domain, overwrite = FALSE)
```

## Arguments

- domain:

  Domain name.

- overwrite:

  Whether to overwrite existing IDs.

## Value

Invisibly returns the registered IDs.

## Details

Equivalent to `pack_topology_a0(domain)` followed by
[`register_rule()`](https://zdz2101.github.io/aRtifacts/reference/register_rule.md)
on each returned rule.
