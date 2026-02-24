# AGENTS

## Purpose
This guide describes the current Core50 architecture for the `aRtifacts` package and the implementation conventions expected for rule development, testing, and documentation.

## Current Architecture (Core50)

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
- `compare_flag_states()`
- `register_rule()`
- `get_rule()`
- `list_rules()`
- `clear_rule_registry()`
- `pack_core_50()`

### Topology packs
- `pack_core_50_schema()` (A0)
- `pack_core_50_a1()`
- `pack_core_50_a2()`
- `pack_core_50_a3()`
- `pack_core_50_a4()`
- `pack_core_50_a5()`
- `pack_core_50_a6()`
- `pack_core_50_a7()`

`run_rules()` dispatches by topology (`A0` to `A7`) using fields on each `CoreRule`.

## File and Naming Conventions

### Rule files
- One rule constructor per file.
- Naming pattern: `R/rule_<family>_<id>_<slug>.R`
- Constructor naming pattern: `rule_<family>_<id>_<slug>()`

Examples:
- `R/rule_sch_001_new_columns_added.R`
- `R/rule_xdom_006_visit_mismatch_across_domains.R`

### Pack files
- One file per topology pack: `R/core50_rules_<topology>.R`
- Each pack file exports:
  - `pack_core_50_<topology>()`
  - `register_core_50_<topology>()`

## Rule Design Contract

Each rule constructor must return a `CoreRule` via `new_rule()` with:
- stable `id` and human-readable metadata,
- declared `topology`,
- appropriate identity/match/delta specs when needed,
- an `fn` evaluator matching dispatcher input for the rule topology.

Rule evaluation should be deterministic, side-effect free, and scoped to the declared topology.

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
- representative topology behavior,
- one-file-per-rule tests in `test-rule_*.R` with minimal inline fixtures.

Prefer direct `new_domain_snapshot()` / `new_artifact_snapshot()` setup in each rule test. Use synthetic keys (for example, `.ROWID`) only when needed to allow intentional business-key violations.

Use minimal fixtures and explicit expectations. Avoid coupling tests to transient ordering unless ordering is part of the contract.

## Documentation Expectations

User-facing docs should reference current exported APIs (`new_rule()`, `run_rules()`, `compare_flag_states()`, `pack_core_50*()`, registry helpers).

For longitudinal findings, document and use `compare_flag_states()` instead of non-exported or removed legacy helpers.

Internal docs should avoid references to removed legacy APIs and modules.

## Migration Status

The package is in Core50-only mode:
- legacy per-rule files and wrappers were removed,
- legacy exports were removed,
- examples were migrated to Core50 execution patterns.

## Notes for Contributors

- Keep changes surgical and topology-local when possible.
- Prefer extending existing helper utilities over introducing parallel abstractions.
- Preserve one-rule-per-file structure.
- Do not reintroduce compatibility aliases unless explicitly requested.
