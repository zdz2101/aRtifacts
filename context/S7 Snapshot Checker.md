# Clinical Data Artifact & Observability Platform

## S7-Based Snapshot, Diff, and Rule Engine Architecture

------------------------------------------------------------------------

## 1. Executive Overview

This platform provides:

-   Typed **data artifacts** (snapshots of SDTM/ADaM domains)
-   Deterministic **week-to-week diffing**
-   Configurable **rule-based anomaly detection**
-   Lightweight **operational memory**
-   Dependency-light architecture
-   CI-native automation (GitHub Actions ready)

It does **not** replace SDTM/ADaM derivation engines.

Instead, it provides:

> Observability, integrity tracking, and operational intelligence across
> time.

------------------------------------------------------------------------

## 2. Core Design Principles

1.  Dependency-light (S7 + base R only in core)
2.  Deterministic, reproducible snapshots
3.  Strong typing at artifact boundaries
4.  Generic diff functions
5.  Modular rule packs
6.  Optional storage and reporting adapters
7.  CI-native workflow
8.  Enterprise-friendly footprint

------------------------------------------------------------------------

## 3. Platform Architecture (Layered)

    ┌────────────────────────────┐
    │      Reporting Layer       │
    │  Quarto / Shiny / HTML     │
    └─────────────▲──────────────┘
                  │
    ┌─────────────┴──────────────┐
    │    Storage Adapters        │
    │  Parquet / DuckDB / JSON   │
    └─────────────▲──────────────┘
                  │
    ┌─────────────┴──────────────┐
    │  Diff + Rule Engine Layer  │
    │  - diff_keys()             │
    │  - diff_values()           │
    │  - diff_meta()             │
    │  - run_rules()             │
    │  - compare flag states     │
    └─────────────▲──────────────┘
                  │
    ┌─────────────┴──────────────┐
    │     S7 Artifact Layer      │
    │  - DomainSnapshot          │
    │  - ArtifactSnapshot        │
    └────────────────────────────┘

Core engine = S7 + base R\
Everything else = optional adapters.

------------------------------------------------------------------------

## 4. Core S7 Objects

### 4.1 DomainSnapshot

Represents a single domain at a specific data cut.

``` r
DomainSnapshot(
  name         = "ADVS",
  data         = data.frame(...),
  key_vars     = c("USUBJID","VISITN","PARAMCD"),
  spec_version = "spec-0.1",
  data_cut     = "2026-02-22",
  build_id     = "gha-run-002",
  code_sha     = "abc123",
  inputs_hash  = "hash_v2"
)
```

### Guarantees

-   Key variables exist
-   Keys are unique
-   Metadata attached
-   Type contract enforced
-   Structural invariants validated

------------------------------------------------------------------------

### 4.2 ArtifactSnapshot

Represents full study snapshot across domains.

``` r
ArtifactSnapshot(
  domains      = list(ADVS = dom, ADAE = dom2),
  spec_version = "spec-0.1",
  data_cut     = "2026-02-22",
  build_id     = "gha-run-002",
  code_sha     = "abc123",
  inputs_hash  = "hash_v2"
)
```

### Guarantees

-   Domains validated
-   Metadata consistent across domains
-   Single source of truth for snapshot lineage

------------------------------------------------------------------------

## 5. Core Diff Engine

### 5.1 Key Diff

``` r
diff_keys(old_dom, new_dom)
```

Example output:

    $added
      USUBJID VISITN PARAMCD
      SUBJ004 1      SYSBP

    $removed
      SUBJ003 1      SYSBP

------------------------------------------------------------------------

### 5.2 Value Diff

``` r
diff_values(old_dom, new_dom, vars = "AVAL")
```

Example output:

    USUBJID  VISITN  PARAMCD  var   old   new
    SUBJ002  2       SYSBP    AVAL  128   135

------------------------------------------------------------------------

### 5.3 Metadata Diff

``` r
diff_meta_artifact(old_art, new_art)
```

Example output:

    field        old         new        changed
    build_id     run-001     run-002    TRUE
    code_sha     aaa111      bbb222     TRUE
    inputs_hash  hash_v1     hash_v2    TRUE

------------------------------------------------------------------------

## 6. Rule Engine

### Rule Structure

``` r
FlagRule(
  id         = "DATE_ORDER",
  domain     = "ADAE",
  severity   = "critical",
  category   = "data_entry_error",
  owner_team = "DM",
  fn         = function(df) { ... }
)
```

### Running Rules

``` r
results <- run_rules(new_rule_context(current = artifact), rules)
```

Example output:

    USUBJID DOMAIN RULE_ID          SEVERITY CATEGORY
    SUBJ010 ADAE   DATE_ORDER       critical data_entry_error
    SUBJ003 ADVS   IMPLAUSIBLE_SYS  warning  clinical_review

------------------------------------------------------------------------

## 7. Longitudinal Flag Intelligence

``` r
compare_flag_states(old_flags, new_flags, by = c("USUBJID", "DOMAIN", "RULE_ID"))
```

Example output:

    introduced:
      SUBJ004 SYSBP_IMPLAUSIBLE

    resolved:
      SUBJ002 DATE_ORDER

    persisting:
      SUBJ010 DATE_ORDER

------------------------------------------------------------------------

## 8. Weekly CI Workflow

Example GitHub Actions trigger:

``` yaml
on:
  schedule:
    - cron: "0 15 * * 1"
```

Output structure:

    artifacts/
      cut=2026-02-22/
        artifact.json
        diff_keys.csv
        diff_values.csv
        flags.csv

------------------------------------------------------------------------

## 9. Optional Storage & Reporting Adapters

Core:

    Imports:
        S7

Optional:

    Suggests:
        arrow,
        duckdb,
        jsonlite,
        yaml,
        quarto,
        testthat

Adapters are guarded with:

``` r
requireNamespace("arrow", quietly=TRUE)
```

------------------------------------------------------------------------

## 10. Value Proposition

This platform provides:

-   Snapshot lineage
-   Deterministic diffing
-   Operational anomaly detection
-   Protocol-aware checks
-   Metadata traceability
-   CI-native automation

Built on:

> S7 + base R core

Lightweight. Extensible. Enterprise-friendly.
