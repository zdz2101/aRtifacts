# CLAUDE

## Purpose
This guide describes the current Topology architecture for the `aRtifacts` package and the implementation conventions expected for rule development, testing, and documentation.

## Package Boundary

- `aRtifacts` is the neutral topology engine.
- `aRtifacts.clinical` contains trial-specific rules, checkers, sample data, and SDTM/ADaM context materials.
- Keep core implementation and docs domain-neutral.

## Current Architecture (Topology)

### Core S7 classes
- `DomainSnapshot`
- `ArtifactSnapshot`
- `CoreRule`
- `RuleContext`
- `IdentitySpec`
- `MatchSpec`
- `DeltaSpec`

### Core execution API
- `new_rule()`
- `run_rules()`
- `register_rule()`
- `get_rule()`
- `list_rules()`
- `clear_rule_registry()`
- `pack_topology_all()`
- `rule_catalog_core()`

### Topology packs
- `pack_topology_a0()` (A0)
- `pack_topology_a1()`
- `pack_topology_a2()`
- `pack_topology_a3()`
- `pack_topology_a4()`
- `pack_topology_a5()`
- `pack_topology_a6()`
- `pack_topology_a7()`

`run_rules()` dispatches by topology (`A0` to `A7`) using fields on each `CoreRule`.

## File and Naming Conventions

### Rule files
- One rule constructor per file.
- Naming pattern: `R/rule_<family>_<id>_<slug>.R`
- Constructor naming pattern: `rule_<family>_<id>_<slug>()`

Examples:
- `R/rule_sch_001_new_columns_added.R`
- `R/rule_rel_001_missing_reference_records.R`

### Pack files
- One file per topology pack: `R/topology_rules_<topology>.R`
- Each pack file exports:
  - `pack_topology_<topology>()`
  - `register_topology_<topology>()`

## Rule Design Contract

Each rule constructor must return a `CoreRule` via `new_rule()` with:
- stable `id` and human-readable metadata,
- declared `topology`,
- appropriate identity/match/delta specs when needed,
- an `fn` evaluator matching the topology payload contract used by `run_rules()`.

The `fn` implementation should be deterministic, side-effect free, and scoped to the declared topology.

## Development Workflow

1. Add or update a rule constructor in its own file.
2. Wire the constructor into the matching topology pack.
3. Ensure rule metadata appears in coverage/catalog expectations.
4. Add or update tests.
5. Update vignette examples only when public usage changes.

## Testing Expectations

Core tests live in `tests/testthat/` and should focus on:
- class constructors and validation,
- execution dispatch behavior (`run_rules()`),
- pack coverage and catalog alignment,
- representative topology behavior.

Use minimal fixtures and explicit expectations. Avoid coupling tests to transient ordering unless ordering is part of the contract.

## Documentation Expectations

User-facing docs should only reference Topology APIs (`new_rule()`, `run_rules()`, `pack_topology_all()`, `pack_topology_a*()`, `register_topology_a*()`, registry helpers).

Internal docs should avoid references to removed legacy APIs/modules and keep trial-specific guidance in `aRtifacts.clinical`.

## Migration Status

The package is in Topology-only mode:
- legacy per-rule files and wrappers were removed,
- legacy exports were removed,
- examples were migrated to Topology execution patterns.

## Notes for Contributors

- Keep changes surgical and topology-local when possible.
- Prefer extending existing helper utilities over introducing parallel abstractions.
- Preserve one-rule-per-file structure.
- Do not reintroduce compatibility aliases unless explicitly requested.
