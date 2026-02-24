# aRtifacts <img src="man/figures/logo.png" align="right" height="139" alt="" />

> **A lightweight, dependency-minimal dataset observability engine for R.**

`aRtifacts` gives you a structured, reproducible way to track what changes in
your data between runs — and to enforce data-quality rules throughout the
lifecycle of a study or pipeline.

The core is wholly **schema-agnostic**: it works with any tabular data that has
a defined key structure, clinical or otherwise. Bundled starter modules add
SDTM- and ADaM-oriented checks on top and are designed to graduate into
dedicated companion packages (`aRtifacts.sdtm`, `aRtifacts.adam`) as the
ecosystem matures.

---

## Installation

```r
# Development version from source
devtools::install_local("path/to/aRtifacts")
```

---

## What it does

| Capability | Description |
|---|---|
| **Snapshot** | Capture a set of keyed tables at a point in time as immutable `DomainSnapshot` / `ArtifactSnapshot` objects |
| **Diff** | Deterministically compare two snapshots — key additions/removals, cell-level value changes, and build metadata |
| **Flag** | Compose and execute data-quality rules (`FlagRule`) against any domain; the engine attaches rule metadata to every flagged row |
| **Track** | Classify flags across data cuts as *introduced*, *persisting*, or *resolved* using `diff_flags()` |
| **Check** | Run structural and temporal checks against the built-in 50-rule observability catalog, or against ADaM (ADSL / BDS) conventions |
| **Core50** | Execute topology-dispatched Core50 rules (`A0`-`A7`) with `run_rules()` |

---

## Quick start

```r
library(aRtifacts)

# --- 1. Snapshot two data cuts ---------------------------------------------------
snap_v1 <- new_domain_snapshot(
  name     = "ADVS",
  data     = example_advs(),
  key_vars = c("USUBJID", "PARAMCD", "AVISIT")
)

advs_v2 <- example_advs()
advs_v2$AVAL[advs_v2$USUBJID == "STUDY01-002" & advs_v2$AVISIT == "WEEK 4"] <- 195

snap_v2 <- new_domain_snapshot("ADVS", advs_v2, key_vars = c("USUBJID", "PARAMCD", "AVISIT"))

# --- 2. See what changed ---------------------------------------------------------
diff_values(snap_v1, snap_v2, vars = "AVAL")
#>       USUBJID PARAMCD AVISIT  var old new
#>   STUDY01-002   SYSBP WEEK 4 AVAL 136 195

# --- 3. Flag it ------------------------------------------------------------------
art_v2 <- new_artifact_snapshot(domains = list(ADVS = snap_v2), build_id = "cut-2")

rule_high_bp <- new_rule(
  id = "TMP-900",
  title = "High systolic BP",
  topology = "A1",
  scope = "record",
  intent = "clinical_review",
  domain = "ADVS",
  owner_team = "Medical Monitor",
  fn = function(data, context, rule) {
    data[!is.na(data$AVAL) & data$PARAMCD == "SYSBP" & data$AVAL > 180, ]
  }
)

ctx <- new_rule_context(current = art_v2)
run_rules(ctx, list(rule_high_bp))

# --- 3b. Core50 packaged path (A1 example) --------------------------------------
core_rule <- rule_id_001_missing_key_fields(
  domain = "ADVS",
  key_vars = c("USUBJID", "PARAMCD", "AVISIT")
)

run_rules(ctx, list(core_rule))

# --- 4. Confirm ADaMIG conformance -----------------------------------------------
check_bds(advs_v2)   # returns empty issue table — dataset is still valid
```

---

## Package contents

### Core engine *(schema-agnostic)*

| Object / Function | Purpose |
|---|---|
| `DomainSnapshot` / `new_domain_snapshot()` | Keyed, validated point-in-time table snapshot |
| `ArtifactSnapshot` / `new_artifact_snapshot()` | Study-level bundle of domain snapshots with build metadata |
| `CoreRule` / `new_rule()` | Topology-aware Core50 rule definition (`A0`-`A7`) |
| `diff_keys()` | Key-combination additions and removals between two snapshots |
| `diff_values()` | Cell-level value changes on matched keys |
| `diff_meta_artifact()` | Build metadata comparison between two artifacts |
| `run_rules()` | Execute topology-dispatched `CoreRule`s with `RuleContext` |
| `diff_flags()` | Longitudinal flag classification (introduced / persisting / resolved) |

### Generalized rule catalog

| Function | Purpose |
|---|---|
| `rule_catalog_core()` | Reference table of 50 high-ROI observability rules |
| `pack_core_50_a1()` ... `pack_core_50_a7()` | Build axis-based Core50 packs by topology |

### Bundled ADaM module *(supplemental — future `aRtifacts.adam`)*

| Function | Purpose |
|---|---|
| `check_adsl()` | Required variables, USUBJID uniqueness, population flags |
| `check_bds()` | Required variables, treatment presence, PARAM/PARAMCD mapping, baseline/timing |
| `validate_adam()` | Convenience wrapper running both checks |

### Example datasets

`example_adsl()` · `example_advs()` · `example_sdtm_lb()`

---

## Vignettes

| Vignette | What it covers |
|---|---|
| [Getting Started](vignettes/getting-started.Rmd) | End-to-end schema-agnostic walkthrough |
| [ADaM Module](vignettes/adam-module.Rmd) | ADaMIG conformance checks for ADSL and BDS |
| [SDTM Module](vignettes/sdtm-module.Rmd) | SDTM checks via Core50 axis packs |
| [Future Extensions](vignettes/future-extensions.Rmd) | Roadmap and planned capabilities |

---

## Design principles

1. **One hard dependency** — `S7` only; everything else is base R.
2. **Always use `S7::S7_inherits()`** for class checks — never `inherits(x, "ClassName")` (S7 prefixes class names with the package namespace on install).
3. **Consistent issue table schema** — every checker returns `data.frame(check, severity, message, n)`.
4. **Pure functions** — no disk writes, no global state, no network calls.

---

## Development

```r
devtools::document()      # regenerate man/ and NAMESPACE
devtools::test()          # run the testthat suite
pkgdown::build_site()     # build the reference site
devtools::check()         # full R CMD check
```
