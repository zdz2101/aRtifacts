# Core Rule Catalog Context

## Purpose
This file defines how to reason about the neutral core starter catalog in `aRtifacts` after the package split.

## Core Scope (aRtifacts)
- Topology-first, domain-neutral validation engine.
- Starter rule packs across `A0` through `A7`.
- No trial-specific semantics, SDTM-only assumptions, or ADaM-only assumptions.

## Companion Scope (aRtifacts.clinical)
- Clinical trial checkers and clinical rule packs.
- SDTM/ADaM implementation-guide context and trial-specific examples.
- Any domain logic that depends on clinical standards.

## Catalog Construction
`rule_catalog_core()` is generated from topology starter packs, not from a fixed cardinality target.

Current source packs:
- `pack_topology_a0()`
- `pack_topology_a1()`
- `pack_topology_a2()`
- `pack_topology_a3()`
- `pack_topology_a4()`
- `pack_topology_a5()`
- `pack_topology_a6()`
- `pack_topology_a7()`

The count can evolve as starter rules are added/removed; do not encode assumptions like `R001..R050`.

## Family Coverage (Core)
- `SCH`: schema checks (`A0`)
- `ID`, `TMP`, `DE`: single-domain record checks (`A1`)
- `ID`, `GRP`: single-domain aggregate checks (`A2`)
- `REL`: cross-domain relationship checks (`A3`)
- `AGG`: cross-domain aggregate checks (`A4`)
- `SNAP`: same-domain longitudinal checks (`A5`, `A6`)
- `HYB`: cross-domain longitudinal checks (`A7`)

## Developer Guidance
- Validate core quality via schema/uniqueness/consistency assertions, not fixed rule counts.
- Keep generic starter rules in core; move standard-specific constraints to `aRtifacts.clinical`.
- When adding or changing rules, update topology packs and let `rule_catalog_core()` reflect the result.
