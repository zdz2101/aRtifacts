# AGENTS

## Purpose

This file is the execution playbook for humans and coding agents who
need to add or extend rule packs quickly in `aRtifacts`.

## Package Boundary

- `aRtifacts` is the topology-first, domain-neutral core.
- `aRtifacts.clinical` holds trial-specific semantics and
  implementation-guide content.
- Do not add SDTM/ADaM or other clinical semantics to `aRtifacts` core
  APIs, vignettes, or starter packs.

## Source Of Truth In This Repo

- Topology dispatch and registry: `R/topology_engine.R`
- Topology decision mapping: `R/topology_decision.R`
- Core classes and constructors: `R/classes.R`, `R/topology_classes.R`
- Topology packs: `R/topology_rules_schema.R`, `R/topology_rules_a1.R`,
  `R/topology_rules_a2.R`, `R/topology_rules_a3.R`,
  `R/topology_rules_a4.R`, `R/topology_rules_a5.R`,
  `R/topology_rules_a6.R`, `R/topology_rules_a7.R`
- Rule-builder scaffold pipeline: `R/rule_builder_spec.R`,
  `R/rule_builder_render.R`, `R/rule_builder_guided_helpers.R`,
  `R/rule_builder_launcher.R`
- Vignette flow and payload contracts: `vignettes/topology-a0-a7.Rmd`,
  `vignettes/add-rule-pack.Rmd`, `vignettes/getting-started.Rmd`

## Instant Rule-Pack Construction Protocol

When asked to “build a rule pack”, run this exact sequence:

1.  Resolve topology using
    `choose_topology(is_longitudinal, n_domains, level)` or the A0-A7
    decision flow in `vignettes/topology-a0-a7.Rmd`.
2.  Pick the smallest payload topology that can express the check.
3.  Create one rule constructor per file in
    `R/rule_<family>_<id>_<slug>.R`.
4.  Return a `CoreRule` via `new_rule(...)` with stable ID, metadata,
    and topology-correct `fn` signature.
5.  Wire the constructor into the matching `pack_topology_a*()` in the
    matching `R/topology_rules_*.R` file.
6.  Add or update a focused
    `tests/testthat/test-rule_<family>_<id>_<slug>.R`.
7.  Run targeted pack/catalog/dispatch tests before finishing.

## Topology Dispatch Contract (Must Match Exactly)

| Topology | Rule shape                           | `fn` signature used by [`run_rules()`](https://zdz2101.github.io/aRtifacts/reference/run_rules.md) |
|----------|--------------------------------------|----------------------------------------------------------------------------------------------------|
| A0       | single-cut schema/snapshot           | `fn(domain, context, rule)` or `fn(context, rule)`                                                 |
| A1       | single-cut single-domain record      | `fn(data, context, rule)`                                                                          |
| A2       | single-cut single-domain aggregate   | `fn(data, context, rule)`                                                                          |
| A3       | single-cut cross-domain relational   | `fn(domains, context, rule)`                                                                       |
| A4       | single-cut cross-domain aggregate    | `fn(domains, context, rule)`                                                                       |
| A5       | longitudinal single-domain record    | `fn(previous, current, context, rule)`                                                             |
| A6       | longitudinal single-domain aggregate | `fn(previous, current, context, rule)`                                                             |
| A7       | longitudinal cross-domain            | `fn(previous, current, context, rule)`                                                             |

[`run_rules()`](https://zdz2101.github.io/aRtifacts/reference/run_rules.md)
skips a rule when required domains are missing. Rule functions must be
deterministic, side-effect free, and return a `data.frame` (or empty
[`data.frame()`](https://rdrr.io/r/base/data.frame.html)).

## Core Object Contract

- Use these classes: `DomainSnapshot`, `ArtifactSnapshot`,
  `RuleContext`, `CoreRule`, `IdentitySpec`, `MatchSpec`, `DeltaSpec`.
- For A1/A2/A5/A6,
  [`new_rule()`](https://zdz2101.github.io/aRtifacts/reference/new_rule.md)
  requires `domain`.
- For A3/A4/A7,
  [`new_rule()`](https://zdz2101.github.io/aRtifacts/reference/new_rule.md)
  requires `domains` with at least two names.
- Keep IDs stable and semantic; do not change behavior under an existing
  ID.
- Core starter IDs follow `^[A-Z]+-[0-9]{3}$` (for example, `REL-004`,
  `SNAP-011`).

## File And Naming Conventions

- One constructor per file.
- File pattern: `R/rule_<family>_<id>_<slug>.R`
- Constructor pattern: `rule_<family>_<id>_<slug>()`
- Family prefixes in core: `SCH`, `ID`, `TMP`, `DE`, `GRP`, `REL`,
  `AGG`, `SNAP`, `HYB`.
- One pack file per topology: `R/topology_rules_<topology>.R` (A0 is in
  `R/topology_rules_schema.R`).
- Each pack file exports both `pack_topology_a*()` and
  `register_topology_a*()`.

## Wiring Contract For Starter Packs

- A0 lives in `pack_topology_a0(domain)` and returns `SCH-001..003`.
- A1 lives in `pack_topology_a1(...)` and always includes `ID-001`, with
  optional `TMP-*` and `DE-*` by args.
- A2 lives in `pack_topology_a2(...)` and includes `ID-002..004`,
  `GRP-001..004`, plus optional `GRP-005`.
- A3/A4/A5/A6/A7 each return fixed starter lists from their topology
  file.
- Keep rule order deterministic in each pack list.
- Keep registry helpers thin wrappers around `pack_topology_a*()` +
  [`register_rule()`](https://zdz2101.github.io/aRtifacts/reference/register_rule.md).

## Fast Scaffold Paths (Use These First)

Use one of these two paths to construct pack scaffolds instantly.

### Path 1: Rule Builder API

1.  Build normalized spec with
    `build_rule_pack_spec(pack = ..., rules = ...)`.
2.  Generate R scaffold with `render_rule_pack_scaffold(spec)`.
3.  Generate handoff bundle with
    `render_rule_agent_prompt_bundle(spec)`.
4.  Optional interactive funnel:
    `launch_rule_builder(output_dir = getwd(), dry_run = TRUE)`.
5.  Expected generated artifacts (timestamped folder):
    `rule-pack.spec.yaml`, `rule_pack_<slug>.R`,
    `agent_prompt_rule_pack_<slug>.md`.

### Path 2: Static Templates

- Use `inst/templates/rule-pack-template.R`.
- Use `inst/templates/topology-rule-template-a0.R` through
  `inst/templates/topology-rule-template-a7.R`.
- Use `inst/templates/template-rule-pack-test.R` for a quick test
  skeleton.

## Testing Gates (Definition Of Done)

Always run targeted tests that prove pack wiring and catalog coherence.

1.  New or changed rule tests: `tests/testthat/test-rule_*.R` for
    touched rules.
2.  Dispatch and pack behavior: `tests/testthat/test-topology-engine.R`.
3.  Constructor coverage: `tests/testthat/test-topology-constructors.R`.
4.  Pack coverage and size expectations:
    `tests/testthat/test-core-rule-coverage.R`.
5.  Registry behavior:
    `tests/testthat/test-topology-registerers-edge.R`,
    `tests/testthat/test-registry-edge.R`.
6.  Catalog alignment: `tests/testthat/test-rule-catalog-core.R`.
7.  Builder/scaffold changes: `tests/testthat/test-rule-builder-*.R`.

If you add or remove starter rules in pack functions, update
expectations in coverage tests accordingly.

## Catalog And Registry Notes

- [`rule_catalog_core()`](https://zdz2101.github.io/aRtifacts/reference/rule_catalog_core.md)
  is computed dynamically from
  [`pack_topology_a0()`](https://zdz2101.github.io/aRtifacts/reference/pack_topology_a0.md)
  to
  [`pack_topology_a7()`](https://zdz2101.github.io/aRtifacts/reference/pack_topology_a7.md).
- No `data-raw` regeneration is required for the current catalog
  implementation.
- [`pack_topology_all()`](https://zdz2101.github.io/aRtifacts/reference/pack_topology_all.md)
  only returns rules currently registered through registry APIs.

## Documentation Rules

- Keep docs aligned to exported APIs in `NAMESPACE`.
- Public usage updates should be reflected in vignettes when behavior
  changes.
- Prefer
  [`compare_flag_states()`](https://zdz2101.github.io/aRtifacts/reference/compare_flag_states.md)
  for longitudinal finding comparisons.
- Avoid references to removed legacy modules/wrappers.

## Agent Prompting Guidance For This Repo

When a user asks for a new rule pack or rule:

1.  Infer topology with
    [`choose_topology()`](https://zdz2101.github.io/aRtifacts/reference/choose_topology.md)
    inputs.
2.  Generate constructor skeleton with correct `fn` signature.
3.  Wire into the right `pack_topology_a*()` immediately.
4.  Add/update rule test file and relevant topology/catalog coverage
    tests.
5.  Run targeted tests and report exact pass/fail scope.
6.  Keep edits topology-local and avoid parallel abstractions when
    existing helpers already fit.

## Non-Negotiables

- Keep one-rule-per-file.
- Keep rule IDs stable.
- Do not reintroduce compatibility aliases unless explicitly requested.
- Prefer extending existing `.topology_*` helpers over duplicating
  utility logic.
