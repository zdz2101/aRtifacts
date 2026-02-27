# aRtifacts <img src="man/figures/logo.png" align="right" height="139" alt="" />

> Topology-driven semantic validation engine for structured tabular data.

`aRtifacts` is the neutral core package in the ecosystem. It provides:

- immutable snapshots (`DomainSnapshot`, `ArtifactSnapshot`),
- deterministic diff primitives,
- topology-dispatched rule execution (`A0`-`A7`),
- starter rule packs for each topology.

Domain-specific logic lives in companion packages (for example, `aRtifacts.clinical`).

## Installation

```r
# Development install from source
# devtools::install_local("zdz2101/aRtifacts")
```

## Quick Start

```r
library(aRtifacts)

orders_v1 <- data.frame(
  entity_id = c("E001", "E002", "E003"),
  record_id = c(1, 2, 3),
  value = c(20, 30, 25),
  group_id = c("G1", "G1", "G2"),
  stringsAsFactors = FALSE
)

orders_v2 <- orders_v1
orders_v2$value[orders_v2$entity_id == "E002"] <- 55

snap_v1 <- new_domain_snapshot("PRIMARY", orders_v1, key_vars = c("entity_id", "record_id"))
snap_v2 <- new_domain_snapshot("PRIMARY", orders_v2, key_vars = c("entity_id", "record_id"))

diff_values(snap_v1, snap_v2, vars = "value")

art_v2 <- new_artifact_snapshot(domains = list(PRIMARY = snap_v2), build_id = "cut-2")
ctx <- new_rule_context(current = art_v2)

rule_high_value <- new_rule(
  id = "TMP-900",
  title = "High value",
  topology = "A1",
  scope = "record",
  intent = "ops_anomaly",
  domain = "PRIMARY",
  owner_team = "Operations",
  fn = function(data, context, rule) {
    data[!is.na(data$value) & data$value > 50, c("entity_id", "record_id", "value"), drop = FALSE]
  }
)

run_rules(ctx, list(rule_high_value))
```

## Topology Packs

Bundled starter packs are organized by topology:

- `pack_topology_a0()` / `register_topology_a0()`
- `pack_topology_a1()` / `register_topology_a1()`
- `pack_topology_a2()` / `register_topology_a2()`
- `pack_topology_a3()` / `register_topology_a3()`
- `pack_topology_a4()` / `register_topology_a4()`
- `pack_topology_a5()` / `register_topology_a5()`
- `pack_topology_a6()` / `register_topology_a6()`
- `pack_topology_a7()` / `register_topology_a7()`

For all bundled starter metadata, use `rule_catalog_core()`.

## Topology chooser (quick)

Use this yes/no sequence to choose rule topology:

1. Compare current vs previous snapshot?
2. Uses more than one domain?
3. Is the check schema-only, row-level, or aggregate-level?

Mapping:

- Single-cut + single-domain: schema `A0`, row `A1`, aggregate `A2`
- Single-cut + cross-domain: relational `A3`, aggregate `A4`
- Longitudinal + single-domain: row/value drift `A5`, aggregate drift `A6`
- Longitudinal + cross-domain: `A7`

For full flow and payload contract examples, see vignette `topology-a0-a7`.

## Core API

### Snapshots

- `new_domain_snapshot()`, `new_artifact_snapshot()`
- `DomainSnapshot`, `ArtifactSnapshot`

### Rules

- `choose_topology()`, `new_rule()`, `run_rules()`, `new_rule_context()`
- `CoreRule`, `RuleContext`
- `register_rule()`, `get_rule()`, `list_rules()`, `clear_rule_registry()`

### Diffs

- `diff_keys()`, `diff_values()`, `diff_columns()`, `diff_column_types()`
- `compare_key_churn()`, `compare_record_volume()`, `compare_distribution_shift()`
- `compare_flag_states()`

## Development

```r
devtools::document()
devtools::test()
pkgdown::build_site()
devtools::check()
```
