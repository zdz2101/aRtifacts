# CLAUDE.md — aRtifacts Package Developer Guide

> **Purpose of this file:** Single authoritative context document for the
> `aRtifacts` R package. Covers architecture, conventions, the complete
> core rule catalog (all 50 rules, implementation status, and design notes),
> and guidelines for extending the package or building supplemental packages
> such as `aRtifacts.sdtm` and `aRtifacts.adam`.
>
> This file is maintained in lockstep with `context/AGENTS.md` so either
> can be used as the primary agent/developer context.
>
> **Using this as AI context for a supplemental package:** Provide this file
> together with the relevant implementation guide (e.g. `SDTMIG_v3.4.md`
> for `aRtifacts.sdtm`, `ADaMIG_v1.3.md` for `aRtifacts.adam`). This file
> supplies the engine contract and rule catalog; the guide supplies the
> domain-specific clinical semantics.

---

## 1. What is aRtifacts?

`aRtifacts` is a **lightweight, dependency-minimal R package** that provides a
**schema-agnostic dataset observability engine**. The core works with any
tabular data that has a defined key structure — clinical, operational, or
otherwise.

It gives programmers and data reviewers a structured way to:

1. **Snapshot** a set of keyed tables at a point in time.
2. **Diff** two snapshots deterministically (keys, values, metadata).
3. **Flag** data-quality issues via a composable rule engine.
4. **Check** datasets against generalized structural rules (core
   catalog).
5. **Track** flags longitudinally across data cuts (introduced / resolved /
   persisting).

SDTM and ADaM checks ship as **bundled supplemental modules** available by
default. These are designed to migrate into dedicated companion packages
(`aRtifacts.sdtm`, `aRtifacts.adam`) as the ecosystem matures.

The package is designed to be a **foundation** — sponsors, study teams, or
non-clinical consumers layer their own domain-specific rule packs on top.

---

## 2. Package identity

| Field | Value |
|---|---|
| Package name | `aRtifacts` |
| Version | `0.1.0` |
| License | MIT |
| R dependency | `>= 4.1` |
| Hard imports | `S7` only |
| Suggests | `knitr`, `rmarkdown`, `pkgdown` |
| Object system | S7 (`S7::new_class`) |
| Documentation | roxygen2 (markdown mode) |
| Site generator | pkgdown (Bootstrap 5) |

---

## 3. Repository layout

```
aRtifacts/
├── DESCRIPTION
├── NAMESPACE               # roxygen2-managed (has header comment)
├── LICENSE
├── README.md
├── aRtifacts.Rproj
├── _pkgdown.yml            # pkgdown site config
├── R/
│   ├── aRtifacts-package.R # Package-level roxygen stub
│   ├── classes.R           # S7 class definitions + constructors (schema-agnostic)
│   ├── diff.R              # Deterministic diff functions (schema-agnostic)
│   ├── rules.R             # Rule execution + longitudinal flag diff (schema-agnostic)
│   ├── generalized_rules.R # Core catalog, generalized checker, make_rule_pack_core wrapper
│   ├── rules_keys.R        # Module 1 — keys & duplicates (R001, R002, R009, R010)
│   ├── rules_dates.R       # Module 2 — date & time consistency (R011–R018)
│   ├── rules_missingness.R # Module 3 — missingness & completeness (R021–R027)
│   ├── rules_plausibility.R # Module 4 — plausibility & range (R031–R039)
│   ├── rules_consistency.R # Module 5 — cross-field & relational consistency (R040–R048)
│   ├── rule_r001_*.R ... rule_r050_*.R # One file per core rule constructor (implemented or NULL template)
│   ├── checkers.R          # Bundled ADaM supplemental checks (ADSL, BDS)
│   ├── sample_data.R       # Synthetic example datasets (SDTM + ADaM supplemental)
│   └── utils.R             # Internal helpers (not exported)
├── tests/testthat/
│   ├── test-rule-r001-*.R ... test-rule-r050-*.R # One test file per core rule constructor
│   └── test-rule-wrapper-integration.R # Verifies wrapper packs match standalone rule constructors
├── man/                    # Auto-generated .Rd files
├── vignettes/
│   ├── getting-started.Rmd  # End-to-end schema-agnostic walkthrough
│   ├── add-rule-pack.Rmd     # How to create and run a custom rule pack
│   ├── adam-module.Rmd      # ADaM conformance checks walkthrough
│   ├── sdtm-module.Rmd      # SDTM tabulation checks walkthrough
│   └── future-extensions.Rmd # Roadmap / planned features
├── inst/
│   └── templates/
│       ├── rule-pack-template.R        # Starter constructor for custom FlagRule packs
│       ├── template-rule-pack-test.R   # Matching testthat starter
│       └── README-rule-templates.md    # Copy/use guidance
└── context/
    ├── CLAUDE.md           # ← You are here
    ├── S7 Snapshot Checker.md  # Original architecture design doc
    ├── ADaMIG_v1.3.md      # ADaM Implementation Guide reference
    ├── SDTMIG_v3.4.md      # SDTM Implementation Guide reference
    └── CORE_RULES.md       # Redirect stub — full rule catalog is now in CLAUDE.md §4.4
```

---

## 4. Architecture at a glance

### 4.1 S7 classes (`R/classes.R`)

| Class | Role | Key properties |
|---|---|---|
| `DomainSnapshot` | One keyed dataset table at one cut | `name`, `data`, `key_vars`, `build_id`, `code_sha`, `inputs_hash` |
| `ArtifactSnapshot` | One or more `DomainSnapshot`s bundled with shared metadata | `domains` (named list), same metadata fields |
| `FlagRule` | Executable data-quality rule targeting one domain | `id`, `domain`, `severity`, `category`, `owner_team`, `fn` |

Each class has a **constructor** (`new_domain_snapshot()`,
`new_artifact_snapshot()`, `new_flag_rule()`) and a built-in **validator**.

### 4.2 Diff engine (`R/diff.R`)

| Function | What it compares |
|---|---|
| `diff_keys(old_dom, new_dom)` | Key-combination adds/removes between two `DomainSnapshot`s |
| `diff_values(old_dom, new_dom, vars)` | Cell-level value changes on matched keys |
| `diff_meta_artifact(old_art, new_art)` | Top-level metadata fields between two `ArtifactSnapshot`s |

### 4.3 Rule engine (`R/rules.R`)

| Function | Purpose |
|---|---|
| `run_flags(artifact, rules)` | Executes a list of `FlagRule`s, returns combined flagged-row table |
| `diff_flags(old_flags, new_flags)` | Classifies flags as introduced / resolved / persisting across cuts |

### 4.4 Generalized checks & core rule catalog (`R/generalized_rules.R` + module files)

Each module wrapper composes standalone per-rule constructors (`rule_r###_*`)
and filters out `NULL` placeholders for rules that are intentionally templated
or not yet implementable in single-snapshot scope.

| Function | Purpose |
|---|---|
| `rule_catalog_core()` | Returns a `data.frame` of all 50 high-ROI observability rules (id, name, scope, category) |
| `check_tabulation_domain(data, ...)` | Runs structural/temporal checks mapped to catalog rules (keys, dates, negatives, visit order, etc.) |
| `make_rule_pack_core(domain, ...)` | Thin wrapper — assembles all 5 module packs into one list |
| `make_rule_pack_keys(domain, ...)` | Module 1: R001, R002, R009, R010 |
| `make_rule_pack_dates(domain, ...)` | Module 2: R011–R018 |
| `make_rule_pack_missingness(domain, ...)` | Module 3: R021–R027 |
| `make_rule_pack_plausibility(domain, ...)` | Module 4: R031–R039 |
| `make_rule_pack_consistency(domain, ...)` | Module 5: R040–R048 |

#### Core rule catalog — all 50 rules

The catalog groups rules into 5 thematic modules. The **`impl`** column
indicates implementation status:

- `✓` — implemented as a `FlagRule` in the named module builder
- `diff` — cross-snapshot check; belongs in the diff engine, not a single-domain rule pack
- `ext` — requires external reference data (controlled terminology table, visit schedule, etc.)
- `custom` — logic is too domain-specific to generalize; implement as a bespoke `FlagRule`
- `—` — not yet implemented

##### Module 1 — Identity, Keys & Duplicates (`R/rules_keys.R`)

| ID | Rule name | Scope | Category | impl |
|---|---|---|---|---|
| R001 | Missing key fields | Record | Data-entry | ✓ `make_rule_pack_keys` |
| R002 | Duplicate keys | Snapshot | Programming/ETL | ✓ `make_rule_pack_keys` |
| R003 | Key type drift | Snapshot | Programming/ETL | `diff` — use `diff_meta_artifact()` |
| R004 | Unexpected key deletions | Snapshot | Programming/ETL | `diff` — use `diff_keys()` |
| R005 | Unexpected key churn spike | Snapshot | Ops-anomaly | `diff` — compare `diff_keys()` counts across cuts |
| R006 | New columns added | Snapshot | Programming/ETL | `diff` — use `diff_meta_artifact()` |
| R007 | Columns removed | Snapshot | Programming/ETL | `diff` — use `diff_meta_artifact()` |
| R008 | Column type drift | Snapshot | Programming/ETL | `diff` — use `diff_meta_artifact()` |
| R009 | High duplicate rate by group | Group | Ops-anomaly | ✓ `make_rule_pack_keys` (param: `group_var`, `dup_rate_threshold`) |
| R010 | High duplicate rate by subject | Group | Ops-anomaly | ✓ `make_rule_pack_keys` (param: `subject_var`, `dup_rate_threshold`) |

##### Module 2 — Date & Time Consistency (`R/rules_dates.R`)

| ID | Rule name | Scope | Category | impl |
|---|---|---|---|---|
| R011 | Start date after end date | Record | Data-entry | ✓ `make_rule_pack_dates` (param: `start_end_pairs`) |
| R012 | Event date outside study period | Record | Data-entry | ✓ `make_rule_pack_dates` (params: `date_vars`, `study_start`, `study_end`) |
| R013 | Invalid or partial date format | Record | Data-entry | ✓ `make_rule_pack_dates` (param: `date_vars`) |
| R014 | End date present but start missing | Record | Data-entry | ✓ `make_rule_pack_dates` (param: `start_end_pairs`) |
| R015 | Time present without date | Record | Data-entry | ✓ `make_rule_pack_dates` (param: `time_date_pairs`) |
| R016 | Visit dates out of order within subject | Group | Data-entry | ✓ `make_rule_pack_dates` (param: `visit_order_var`) |
| R017 | Identical timestamps across multiple records | Group | Ops-anomaly | ✓ `make_rule_pack_dates` (param: `date_vars`) |
| R018 | Future-dated events | Record | Data-entry | ✓ `make_rule_pack_dates` (param: `cutoff_date`, default `Sys.Date()`) |
| R019 | Large unexpected date gaps for scheduled visits | Group | Ops-anomaly | `ext` — requires visit schedule reference |
| R020 | Timezone inconsistency within group | Group | Ops-anomaly | `ext` — requires ISO 8601 datetime strings with UTC offset |

##### Module 3 — Missingness & Completeness Drift (`R/rules_missingness.R`)

| ID | Rule name | Scope | Category | impl |
|---|---|---|---|---|
| R021 | Overall missingness spike | Snapshot | Ops-anomaly | ✓ `make_rule_pack_missingness` (param: `missingness_threshold`) |
| R022 | Missingness spike by group | Group | Ops-anomaly | ✓ `make_rule_pack_missingness` (params: `group_var`, `missingness_threshold`) |
| R023 | Missingness spike by visit/timepoint | Group | Ops-anomaly | ✓ `make_rule_pack_missingness` (param: `visit_var`, `missingness_threshold`) |
| R024 | Required variable missingness > 0 | Snapshot | Data-entry | ✓ `make_rule_pack_missingness` (param: `required_vars`) |
| R025 | Expected domain empty | Snapshot | Programming/ETL | ✓ `make_rule_pack_missingness` |
| R026 | Subjects without follow-up after baseline | Group | Ops-anomaly | ✓ `make_rule_pack_missingness` (param: `baseline_flag_var`) |
| R027 | New subjects missing baseline assessments | Group | Ops-anomaly | ✓ `make_rule_pack_missingness` (param: `baseline_flag_var`) |
| R028 | Late-arriving data volume spike | Snapshot | Ops-anomaly | `diff` — compare record counts between snapshot cuts |
| R029 | Unexpected data plateau (no updates) | Snapshot | Ops-anomaly | `diff` — compare `diff_keys()` / `diff_values()` counts to detect no-change cuts |
| R030 | Form completion rate outlier by group | Group | Ops-anomaly | `ext` — requires external form completion log |

##### Module 4 — Plausibility & Range Checks (`R/rules_plausibility.R`)

| ID | Rule name | Scope | Category | impl |
|---|---|---|---|---|
| R031 | Physiologically impossible values | Record | Clinical-review | ✓ `make_rule_pack_plausibility` (param: `value_ranges = list(VAR = c(min, max))`) |
| R032 | Unit mismatch heuristic | Group | Ops-anomaly | `ext` — requires CT unit-to-parameter mapping |
| R033 | Negative values where not allowed | Record | Data-entry | ✓ `make_rule_pack_plausibility` (param: `non_negative_vars`) |
| R034 | Suspicious zero values | Group | Ops-anomaly | ✓ `make_rule_pack_plausibility` (params: `numeric_vars`, `zero_rate_threshold`) |
| R035 | Extreme statistical outliers (IQR) | Record | Clinical-review | ✓ `make_rule_pack_plausibility` (params: `numeric_vars`, `iqr_multiplier`) |
| R036 | Group-level distribution shift | Group | Ops-anomaly | `diff` — requires historical baseline distribution from a prior snapshot |
| R037 | Within-subject implausible change | Group | Clinical-review | ✓ `make_rule_pack_plausibility` (params: `visit_order_var`, `max_within_change`) |
| R038 | Too-perfect data (low variance / CV) | Group | Ops-anomaly | ✓ `make_rule_pack_plausibility` (params: `numeric_vars`, `low_cv_threshold`) |
| R039 | Rounding pattern anomaly | Group | Ops-anomaly | ✓ `make_rule_pack_plausibility` (params: `numeric_vars`, `round_rate_threshold`) |

##### Module 5 — Cross-Field & Relational Consistency (`R/rules_consistency.R`)

| ID | Rule name | Scope | Category | impl |
|---|---|---|---|---|
| R040 | Impossible categorical combinations | Record | Data-entry | ✓ `make_rule_pack_consistency` (param: `impossible_combos`) |
| R041 | Inconsistent categorical pairs | Record | Data-entry | ✓ `make_rule_pack_consistency` (param: `inconsistent_pairs`) |
| R042 | Derived flag inconsistent with source data | Record | Programming/ETL | `custom` — implement per-domain as a bespoke `FlagRule` |
| R043 | Placeholder text prevalence spike | Record | Ops-anomaly | ✓ `make_rule_pack_consistency` (param: `placeholder_patterns`) |
| R044 | Controlled terminology drift within group | Group | Ops-anomaly | `ext` — requires CT reference table |
| R045 | Unexpected explosion in distinct categorical values | Snapshot | Ops-anomaly | ✓ `make_rule_pack_consistency` (params: `categorical_vars`, `max_distinct_values`) |
| R046 | Child record without parent record | Record | Data-entry | `ext` — requires cross-domain parent data |
| R047 | Sequence gaps or duplicate sequence numbers | Group | Data-entry | ✓ `make_rule_pack_consistency` (param: `seq_vars`) |
| R048 | Record count per subject outlier | Group | Ops-anomaly | ✓ `make_rule_pack_consistency` (param: `record_count_iqr_multiplier`) |
| R049 | Enrollment vs assessment mismatch by group | Group | Ops-anomaly | `diff` — requires cross-domain enrollment data |
| R050 | Record deletions without associated code/spec change | Snapshot | Programming/ETL | `diff` — compare `diff_keys()` deletions against spec-change log |

### 4.5 Bundled ADaM module (`R/checkers.R`)

> **Supplemental.** These will migrate to a companion `aRtifacts.adam` package.

| Function | Purpose |
|---|---|
| `check_adsl(adsl)` | Required vars, USUBJID uniqueness, population flags |
| `check_bds(bds)` | Required vars, treatment presence, PARAM/PARAMCD mapping, baseline/timing |
| `validate_adam(adsl, bds)` | Convenience wrapper running both |

### 4.6 Example data (`R/sample_data.R`)

| Function | Data type | Module |
|---|---|---|
| `example_adsl()` | Synthetic ADSL (3 subjects) | ADaM supplemental |
| `example_advs()` | Synthetic ADVS / BDS vital signs | ADaM supplemental |
| `example_sdtm_lb()` | Synthetic SDTM LB domain | SDTM supplemental |

### 4.7 Internal helpers (`R/utils.R`)

Not exported. Includes: `%||%`, `.make_issue()`, `.empty_issues()`,
`.bind_issues()`, `.key_id()`, `.has_unique_keys()`, `.is_one_to_one()`,
`.assert_domain()`, `.assert_artifact()`.

All issue-producing functions return `data.frame`s with columns: `check`,
`severity`, `message`, `n`.

---

## 5. How to build and validate

```r
# Generate documentation (writes man/ and updates NAMESPACE)
devtools::document()

# Run the test suite
devtools::test()

# Run examples (quick smoke-test without the full suite)
devtools::run_examples()

# Build pkgdown site
pkgdown::build_site()

# Full R CMD check
devtools::check()
```

> **Note:** `NAMESPACE` is roxygen2-managed. Do not hand-edit it—run
> `devtools::document()` instead.

---

## 5a. Test suite

Unit tests live in `tests/testthat/` and use **testthat 3** edition.

| File | What it covers |
|---|---|
| `helper-data.R` | Shared fixtures (`.df_orders()`, `.dom_orders()`, `.art_orders()`, `.rule_high_amount()`) sourced automatically before every test file |
| `test-classes.R` | `new_domain_snapshot()`, `new_artifact_snapshot()`, `new_flag_rule()` — construction, property storage, validator errors |
| `test-diff.R` | `diff_keys()`, `diff_values()`, `diff_meta_artifact()` — happy paths, edge cases, error guards |
| `test-rules.R` | `run_flags()`, `diff_flags()` — execution, metadata columns, multi-rule combining, longitudinal classification |
| `test-generalized-rules.R` | `check_tabulation_domain()` and wrapper-level `make_rule_pack_core()` composition checks |
| `test-rule-catalog-core.R` | `rule_catalog_core()` and packaged dataset integrity/parity checks |
| `test-core-rule-coverage.R` | Implemented-vs-catalog core rule ID coverage assertions |
| `test-rules-keys-module.R` | Module 1 (`make_rule_pack_keys()`) behavior checks |
| `test-rules-dates-module.R` | Module 2 (`make_rule_pack_dates()`) behavior checks |
| `test-rules-missingness-module.R` | Module 3 (`make_rule_pack_missingness()`) behavior checks |
| `test-rules-plausibility-module.R` | Module 4 (`make_rule_pack_plausibility()`) behavior checks |
| `test-rules-consistency-module.R` | Module 5 (`make_rule_pack_consistency()`) behavior checks |
| `test-rule-r001-*.R` ... `test-rule-r050-*.R` | Standalone per-rule constructor tests (implemented rules use fixture-based trigger assertions; placeholder rules assert `NULL`) |
| `test-rule-wrapper-integration.R` | Wrapper-to-standalone parity (wrapper outputs equal direct `rule_r###_*()` construction after filtering `NULL`) |
| `test-checkers.R` | `check_adsl()`, `check_bds()`, `validate_adam()` |
| `test-sample-data.R` | `example_adsl()`, `example_advs()`, `example_sdtm_lb()` — shape, required columns, key uniqueness |

Run with `devtools::test()`. Target: **0 failures, 0 warnings** (a single
contextual skip for `data-raw` path availability is acceptable in staged
test environments).

---

## 6. Design conventions and rules

1. **Dependency budget.** The only hard import is `S7`. Everything else
   uses base R. Do not introduce tidyverse or data.table as hard
   dependencies without explicit approval.

2. **Issue table schema.** Every checker / validator must return a
   `data.frame(check, severity, message, n)`. Use `.make_issue()` and
   `.bind_issues()` from `utils.R`.

3. **FlagRule contract.** A rule's `fn` receives a plain `data.frame` and
   must return a `data.frame` of flagged rows (or an empty `data.frame`
   if nothing is flagged). The engine adds metadata columns
   (`RULE_ID`, `DOMAIN`, `SEVERITY`, `CATEGORY`, `OWNER_TEAM`).

4. **S7 property access.** Use `@` (e.g., `snapshot@data`), not `$`.

5. **S7 type-checking.** Always use `S7::S7_inherits(x, ClassName)` — never
   `inherits(x, "ClassName")`. When the package is installed, S7 registers
   class names with the package namespace prefix
   (e.g., `"aRtifacts::DomainSnapshot"`), which makes base-R string-based
   `inherits()` always return `FALSE`. Pass the class *object* to
   `S7::S7_inherits()` instead. The helpers `.assert_domain()` and
   `.assert_artifact()` in `utils.R` already do this correctly and should
   be used rather than open-coded checks.

6. **Roxygen style.** Markdown mode is enabled. Cross-references to
   package-internal topics should use backtick code spans (`` `ClassName` ``)
   rather than bracket links (`[ClassName]`), because roxygen2 cannot
   resolve bracket links to S7 class objects that lack standard `.Rd`
   topic aliases. This avoids "Could not resolve link" warnings.

7. **Naming.** Exported constructors are `new_*()`. Exported check
   functions are `check_*()` or `validate_*()`. Internal helpers are
   dot-prefixed (`.helper_name()`).

8. **No runtime side-effects.** Functions are pure: no writing to disk,
   no global state mutation, no network calls.

---

## 7. Context documents (reference material)

These live in `context/` and informed the implementation:

| File | What it contains |
|---|---|
| `S7 Snapshot Checker.md` | Original architecture vision for the snapshot/diff/rule engine |
| `ADaMIG_v1.3.md` | ADaM Implementation Guide — provide alongside this file when developing `aRtifacts.adam` |
| `SDTMIG_v3.4.md` | SDTM Implementation Guide — provide alongside this file when developing `aRtifacts.sdtm` |
| `CORE_RULES.md` | Redirect stub only — full rule catalog with implementation notes is in §4.4 of this file |

When extending the package, **consult these docs** for clinical-domain
rationale before adding or modifying rules.

---

## 8. Extending the package

### Adding a new rule to an existing pack

1. Write a function `fn(df) -> data.frame` implementing the check logic.
2. Call `new_flag_rule(id, domain, fn = fn, ...)` to wrap it.
3. Add it to the appropriate module file (`rules_keys.R`, `rules_dates.R`,
   `rules_missingness.R`, `rules_plausibility.R`, `rules_consistency.R`)
   inside the corresponding `make_rule_pack_*()` builder function.
4. Add `@export` and a roxygen block if the pack builder is public.
5. Run `devtools::document()`.

### Bootstrapping a brand-new custom pack (template path)

Use the starter files in `inst/templates/`:

1. Copy `inst/templates/rule-pack-template.R` into `R/` and rename
   `make_rule_pack_template()`.
2. Replace `TMP...` rule IDs with stable domain-specific IDs.
3. Copy `inst/templates/template-rule-pack-test.R` into `tests/testthat/`
   and adapt expected IDs/behavior.
4. Follow `inst/templates/README-rule-templates.md` for ID/versioning guidance.

### Adding a new domain-specific checker

1. Create a function following the pattern in `checkers.R` (`check_adsl`
   is a clean template).
2. Use `add_issue()` / `.bind_issues()` internally.
3. Return a `data.frame(check, severity, message, n)`.
4. Export it in the roxygen block, then `devtools::document()`.
5. Add the function name to the appropriate section in `_pkgdown.yml`.

### Adding a new S7 class

1. Define the class in `classes.R` with `S7::new_class()`.
2. Include a `validator` function. If the validator checks whether a property
   contains objects of another S7 class, use `S7::S7_inherits(x, OtherClass)`
   — **not** `inherits(x, "OtherClass")` (see convention 5 above).
3. Write a `new_*()` constructor with a roxygen `@export` tag.
4. Export both the class object and the constructor.
5. If other functions need to guard against wrong input types, add a
   `.assert_*()` helper in `utils.R` using `S7::S7_inherits()`, following
   the pattern of `.assert_domain()` and `.assert_artifact()`.
6. Add to `_pkgdown.yml` reference section.

### Creating a new vignette

1. Place an `.Rmd` file in `vignettes/` with the standard YAML header
   (see `getting-started.Rmd` as a template).
2. Register it in the `articles` section of `_pkgdown.yml`.
3. Ensure all code chunks use only exported API + example data generators.

`add-rule-pack.Rmd` is now the canonical example for documenting custom
rule-pack development workflows.

---

## 9. Exported API quick-reference

### Classes
- `DomainSnapshot` / `new_domain_snapshot()`
- `ArtifactSnapshot` / `new_artifact_snapshot()`
- `FlagRule` / `new_flag_rule()`

### Diff
- `diff_keys()`, `diff_values()`, `diff_meta_artifact()`, `diff_flags()`

### Generalized checks
- `rule_catalog_core()`, `check_tabulation_domain()`
- `make_rule_pack_core()` — thin wrapper calling 5 module builders below
- `make_rule_pack_keys()` — R001, R002, R009, R010
- `make_rule_pack_dates()` — R011-R018
- `make_rule_pack_missingness()` — R021-R027
- `make_rule_pack_plausibility()` — R031-R039
- `make_rule_pack_consistency()` — R040-R048

### Rule execution
- `run_flags()`

### Bundled SDTM/ADaM checks (supplemental)
- `check_adsl()`, `check_bds()`, `validate_adam()`

### Example data (supplemental)
- `example_adsl()`, `example_advs()`, `example_sdtm_lb()`

---

## 10. Known limitations and future work

- **Core catalog coverage.** `make_rule_pack_core()` is a thin
   wrapper around 5 module builders that currently implement 33 of the 50
  catalog rules as executable `FlagRule`s. Rules requiring cross-snapshot
  comparison (R003-R008, R028-R030, R050) belong in the diff engine.
  Rules requiring external reference data (R020, R032, R044) and rules
  too domain-specific to generalize (R042) are noted in the catalog but
  not yet enforced programmatically.
- **Companion package separation.** `check_adsl()`, `check_bds()`,
  `validate_adam()`, and related example data are bundled in the core today.
  They are earmarked for extraction into `aRtifacts.adam` (and
  `aRtifacts.sdtm` for SDTM-specific checkers) once the ecosystem
  stabilizes.
- **SDTM-specific checkers.** `check_tabulation_domain()` is generic.
  Domain-specific SDTM checkers (e.g., `check_dm()`, `check_ae()`) have
  not been written; they belong in the future `aRtifacts.sdtm`.
- **No serialization.** Snapshots live in-memory only; persistence to
  disk/database is left to the consumer.
- **pkgdown site.** Config is ready; the site has not been deployed.

---

## 11. Glossary

| Term | Meaning in this package |
|---|---|
| **Domain** | A single keyed dataset table (e.g., ADSL, LB, orders, metrics) |
| **Snapshot** | An immutable point-in-time capture of one or more domains |
| **Artifact** | A study-level bundle of domain snapshots with build metadata |
| **Diff** | A deterministic comparison of two snapshots or artifacts |
| **Flag** | A row-level data-quality finding produced by a `FlagRule` |
| **Rule pack** | A reusable list of `FlagRule` objects targeting a domain |
| **Catalog** | The reference table of 50 high-ROI observability rule definitions |
| **Issue table** | A `data.frame(check, severity, message, n)` returned by checkers |
| **Key vars** | Columns that uniquely identify rows in a domain |
| **BDS** | Basic Data Structure — ADaM dataset class for repeated-measure data |
| **ADSL** | Subject-Level Analysis Dataset — one row per subject |
