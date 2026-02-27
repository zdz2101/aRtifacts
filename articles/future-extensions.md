# Extension Architecture

`aRtifacts` is the neutral topology engine.

## Core responsibilities

- Snapshot classes (`DomainSnapshot`, `ArtifactSnapshot`)
- Diff primitives
- Topology dispatcher (`run_rules`)
- Starter packs (`pack_topology_a0` … `pack_topology_a7`)

## Companion package pattern

Domain-specific ecosystems should depend on `aRtifacts` and add:

- domain rule constructors,
- domain pack builders,
- domain checkers/sample data,
- domain-focused vignettes.

`aRtifacts.clinical` is the reference companion for clinical-trial
logic.

## Recommended contributor workflow

1.  Add one rule constructor per file.
2.  Add/extend a topology pack.
3.  Add a focused test file under `tests/testthat/`.
4.  Regenerate docs (`devtools::document()`).
5.  Run tests and checks.

## Stability policy

- Keep rule IDs stable.
- Avoid changing semantics under existing IDs.
- Add new IDs for new behavior.
