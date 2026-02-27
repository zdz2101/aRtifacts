# Build A0 schema pack

Returns topology starter schema rules.

## Usage

``` r
pack_topology_a0(domain)
```

## Arguments

- domain:

  Domain name.

## Value

A list of `CoreRule` objects.

## Details

A0 rules evaluate schema/snapshot-level change on a single domain and
are typically run with both `current` and `previous` artifacts in
[`new_rule_context()`](https://zdz2101.github.io/aRtifacts/reference/new_rule_context.md).

This pack includes:

- `SCH-001`: new columns added,

- `SCH-002`: columns removed,

- `SCH-003`: column type drift.
