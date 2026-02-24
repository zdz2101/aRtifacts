# aRtifacts --- Master Plan for the Core 50 Rules

*A practical blueprint for how each rule is derived, what it needs, and
where it fits on the 3 axes.*

## What this document is

This doc is the "wiring diagram" for the **Core 50** rule pack in
`aRtifacts`:

-   **How each rule is derived** (the pattern it follows)
-   **Axis placement**:
    -   **Axis A (Topology)** = how it executes\
    -   **Axis B (Scope)** = what level it flags\
    -   **Axis C (Intent)** = why it exists\
-   **Inputs**: domains + identity/match/delta specs
-   **Pseudo-implementation**: enough to implement quickly in
    `eval_A*()`

> Key point: **Rules are instances**, not classes. All 50 are
> `FlagRule(...)` objects. Execution is controlled by `topology` routing
> in the engine.

------------------------------------------------------------------------

## Axes Reference

### Axis A --- Topology (execution shape)

-   **A0 `schema`**: schema signatures, columns/types/labels/codelists
    drift
-   **A1 `intra_domain_row`**: row predicate within one domain
-   **A2 `intra_domain_group`**: group/aggregate within one domain
-   **A3 `cross_domain_relational`**: join/match across domains (same
    cut)
-   **A4 `cross_domain_aggregate`**: compare aggregates across domains
    (same cut)
-   **A5 `cross_snapshot_row`**: diff aligned records across cuts
-   **A6 `cross_snapshot_aggregate`**: compare aggregates across cuts
-   **A7 `cross_domain_cross_snapshot`**: combines cross-domain +
    cross-snapshot (three standard patterns)

### Axis B --- Scope (what gets flagged)

-   **record**: individual rows / entities
-   **group**: grouped rollups (by USUBJID, SITEID, ARM, etc.)
-   **snapshot**: dataset-level anomalies (counts, schema drift, etc.)

### Axis C --- Intent (why it exists)

-   **core_integrity**
-   **data_entry**
-   **clinical_review**
-   **programming_etl**
-   **ops_anomaly**

------------------------------------------------------------------------

## Data Model Assumptions (what rules "consume")

### DomainSnapshot

-   `name`, `data`, `key_vars`, provenance fields
-   `diag` includes: `n`, `schema_signature`, `key_diagnostics`

### ArtifactSnapshot

-   `domains`: named list of DomainSnapshot (e.g., `"DM"`, `"AE"`,
    `"LB"`)
-   `diag` includes: `n_by_domain`, `schema_by_domain`,
    `key_ok_by_domain`

### RuleContext

-   `current` ArtifactSnapshot
-   `previous` ArtifactSnapshot (optional; required for A5/A6/A7)
-   `options` (tolerances, thresholds)

### Specs used by rules

-   **IdentitySpec**: how to align records across snapshots (keys, hash,
    fuzzy)
-   **MatchSpec**: how to match across domains (keys, join type,
    optional time-window logic)
-   **DeltaSpec**: what fields to compare across snapshots + tolerances

------------------------------------------------------------------------

## How rules are "derived" (the 8 templates)

Each of the Core 50 rules is an instance of one of these templates.

### T0 --- Schema drift

**Topology** A0 (or A5/A6 for schema across cuts, but treat as A0
schema-diff) - derive `schema_signature(domain)` - compare
sets/types/labels/codelists

### T1 --- Row predicate

**Topology** A1 - `bad <- predicate(df, params)` - emit flags for
`which(bad)`

### T2 --- Group aggregate threshold

**Topology** A2 - `agg <- group_summarize(df, group_keys, metric)` -
flag groups where `metric` violates threshold

### T3 --- Cross-domain relational (join + predicate)

**Topology** A3 - `joined <- join(A, B, by=MatchSpec$by)` -
`bad <- predicate(joined)`

### T4 --- Cross-domain aggregate reconciliation

**Topology** A4 - `aggA <- summarize(A)` and `aggB <- summarize(B)` -
`bad <- reconcile(aggA, aggB)`

### T5 --- Cross-snapshot row diff (align + classify)

**Topology** A5 - align by `IdentitySpec` - classify
`new/removed/changed` - flag unexpected cases

### T6 --- Cross-snapshot aggregate drift

**Topology** A6 - compute metric by snapshot (and maybe by site) -
compare delta vs thresholds

### T7 --- Cross-domain + cross-snapshot (A7) --- **only 3 valid patterns**

-   **P1 diff→join**: diff one domain first, then join to another
-   **P2 join→diff**: compute relationship within each cut, then compare
    relationships
-   **P3 metric→compare**: derive metrics per cut from multiple domains,
    then compare

------------------------------------------------------------------------

# Core 50 Rule Catalog

Below, each rule lists: - **ID + Title** - **Derived from template** -
**Axis A / B / C** - **Inputs** (domains + specs) - **Pseudo code**

> The IDs are *recommended*; adjust to your preferred naming. The
> important part is the **axis placement** and **inputs**.

------------------------------------------------------------------------

## A0 --- Schema Rules (5)

### SCH-001 New columns added

-   **Template**: T0
-   **Axes**: A0 / snapshot / programming_etl
-   **Inputs**: domain schema in current vs previous (or current vs spec
    baseline)
-   **Pseudo**

``` r
cur <- schema(ctx$current, domain)
prev <- schema(ctx$previous, domain)
added <- setdiff(cur$columns, prev$columns)
flag_if(length(added) > 0, snapshot_flag(added))
```

### SCH-002 Columns removed

-   Template T0
-   Axes A0 / snapshot / programming_etl
-   Pseudo

``` r
removed <- setdiff(prev$columns, cur$columns)
flag_if(length(removed) > 0, snapshot_flag(removed))
```

### SCH-003 Column type drift

-   Template T0
-   Axes A0 / snapshot / programming_etl
-   Pseudo

``` r
drift <- intersect(cur$columns, prev$columns)
bad <- drift[cur$type[drift] != prev$type[drift]]
flag_if(length(bad) > 0, snapshot_flag(bad))
```

### SCH-004 Controlled terminology drift

-   Template T0
-   Axes A0 / snapshot / programming_etl
-   Inputs: (optional) code list metadata or observed unique values
-   Pseudo

``` r
obs_cur <- sort(unique(df_cur[[var]]))
obs_prev <- sort(unique(df_prev[[var]]))
new_levels <- setdiff(obs_cur, obs_prev)
flag_if(length(new_levels) > 0, snapshot_flag(new_levels))
```

### SCH-005 Unexpected label/metadata drift

-   Template T0
-   Axes A0 / snapshot / programming_etl
-   Inputs: variable labels/attributes if available
-   Pseudo

``` r
label_diff <- vars_with_label_changes(cur, prev)
flag_if(length(label_diff) > 0, snapshot_flag(label_diff))
```

------------------------------------------------------------------------

## A1 --- Intra-Domain Row Rules (10)

### ID-001 Missing key fields

-   Template T1
-   Axes A1 / record / core_integrity
-   Inputs: domain + key_vars
-   Pseudo

``` r
bad <- any_na_or_blank(df[key_vars])
emit_row_flags(bad, entity_key="USUBJID")
```

### TMP-001 Invalid date format

-   Template T1
-   Axes A1 / record / data_entry

``` r
bad <- !is_valid_iso_date(df[[date_var]])
emit_row_flags(bad)
```

### TMP-002 Start date after end date

-   Template T1
-   Axes A1 / record / data_entry

``` r
bad <- df[[start]] > df[[end]]
emit_row_flags(bad)
```

### TMP-003 End date without start

-   Template T1
-   Axes A1 / record / data_entry

``` r
bad <- !is.na(df[[end]]) & is.na(df[[start]])
emit_row_flags(bad)
```

### TMP-004 Time without date

-   Template T1
-   Axes A1 / record / data_entry

``` r
bad <- !is.na(df[[time]]) & is.na(df[[date]])
emit_row_flags(bad)
```

### DE-001 Required field missing conditionally

-   Template T1
-   Axes A1 / record / data_entry

``` r
bad <- (df[[cond_var]] == "Y") & is.na(df[[required_var]])
emit_row_flags(bad)
```

### DE-002 Out-of-range numeric values

-   Template T1
-   Axes A1 / record / clinical_review

``` r
bad <- df[[x]] < lo | df[[x]] > hi
emit_row_flags(bad)
```

### DE-003 Invalid categorical code

-   Template T1
-   Axes A1 / record / data_entry

``` r
bad <- !(df[[x]] %in% allowed_levels)
emit_row_flags(bad)
```

### DE-004 Negative duration

-   Template T1
-   Axes A1 / record / clinical_review

``` r
bad <- df[[duration]] < 0
emit_row_flags(bad)
```

### DE-005 Impossible logical combination

-   Template T1
-   Axes A1 / record / clinical_review

``` r
bad <- (df$SEX == "M") & (df$PREGNANT == "Y")
emit_row_flags(bad)
```

------------------------------------------------------------------------

## A2 --- Intra-Domain Group Rules (8)

### ID-002 Duplicate keys

-   Template T2
-   Axes A2 / group / core_integrity

``` r
dups <- df |> group_by(across(key_vars)) |> summarize(n=dplyr::n()) |> filter(n>1)
emit_group_flags(dups, entity_id=paste_keys())
```

### ID-003 High duplicate rate by site

-   Template T2
-   Axes A2 / group / ops_anomaly

``` r
rate <- dup_rate(df, key_vars, group="SITEID")
flag_if(rate > threshold, group_flags_by_site(rate))
```

### ID-004 High duplicate rate by subject

-   Template T2
-   Axes A2 / group / ops_anomaly

``` r
rate <- dup_rate(df, key_vars, group="USUBJID")
flag_if(rate > threshold, group_flags_by_subject(rate))
```

### GRP-001 Excessive missingness rate

-   Template T2
-   Axes A2 / group / data_entry

``` r
miss <- missing_rate(df, vars=target_vars, group="SITEID")
flag_if(miss > threshold, group_flags(miss))
```

### GRP-002 Subject with excessive event frequency

-   Template T2
-   Axes A2 / group / ops_anomaly

``` r
counts <- df |> count(USUBJID) |> filter(n > max_events)
emit_group_flags(counts)
```

### GRP-003 Repeated identical lab values

-   Template T2
-   Axes A2 / group / ops_anomaly

``` r
rep <- LB |> group_by(USUBJID, LBTESTCD) |> summarize(n_unique=n_distinct(LBSTRESN)) |> filter(n_unique==1 & n()>k)
emit_group_flags(rep)
```

### GRP-004 Site-level outlier rate

-   Template T2
-   Axes A2 / group / ops_anomaly

``` r
rate <- event_rate(df, group="SITEID")
flag_outliers(rate, method="robust_z", k=3)
```

### GRP-005 Distribution skew anomaly

-   Template T2
-   Axes A2 / snapshot / ops_anomaly

``` r
skew <- compute_skew(df[[x]])
flag_if(abs(skew) > threshold, snapshot_flag(skew))
```

------------------------------------------------------------------------

## A3 --- Cross-Domain Relational (8)

### XDOM-001 AE without DM

-   Template T3
-   Axes A3 / record / core_integrity
-   Inputs: AE, DM; MatchSpec by USUBJID

``` r
bad <- anti_join(AE, DM, by="USUBJID")
emit_row_flags(bad, entity_key="USUBJID")
```

### XDOM-002 EX without DM

-   Template T3
-   Axes A3 / record / core_integrity

``` r
bad <- anti_join(EX, DM, by="USUBJID")
emit_row_flags(bad)
```

### XDOM-003 LB without expected visit

-   Template T3
-   Axes A3 / record / programming_etl

``` r
bad <- anti_join(LB, VS, by=c("USUBJID","VISIT"))
emit_row_flags(bad)
```

### XDOM-004 CM outside AE window

-   Template T3
-   Axes A3 / record / clinical_review
-   Inputs: AE, CM; MatchSpec by USUBJID + TimeMatchSpec

``` r
cm_ae <- join_by_subject(CM, AE)
bad <- !within_window(cm_ae$CMSTDTC, cm_ae$AESTDTC, cm_ae$AEENDTC, days=7)
emit_row_flags(bad)
```

### XDOM-005 AE outside exposure window

-   Template T3
-   Axes A3 / record / clinical_review

``` r
ae_ex <- join_by_subject(AE, EX)
bad <- !within_exposure(ae_ex$AESTDTC, ae_ex$EXSTDTC, ae_ex$EXENDTC)
emit_row_flags(bad)
```

### XDOM-006 Visit mismatch across domains

-   Template T3
-   Axes A3 / group / programming_etl

``` r
miss <- setdiff(visits(VS), visits(LB))  # per subject or per site
emit_group_flags(miss)
```

### XDOM-007 Orphan records in child domain

-   Template T3
-   Axes A3 / record / core_integrity

``` r
bad <- anti_join(child, parent, by=parent_keys)
emit_row_flags(bad)
```

### XDOM-008 Demographic inconsistency across domains

-   Template T3
-   Axes A3 / record / data_entry

``` r
dm_vs <- inner_join(DM, VS, by="USUBJID")
bad <- dm_vs$SEX != dm_vs$SEX_in_vs  # example field
emit_row_flags(bad)
```

------------------------------------------------------------------------

## A4 --- Cross-Domain Aggregate (4)

### XAGG-001 Subject count mismatch

-   Template T4
-   Axes A4 / snapshot / programming_etl

``` r
n_dm <- n_distinct(DM$USUBJID)
n_ae <- n_distinct(AE$USUBJID)
flag_if(abs(n_dm - n_ae) > tol, snapshot_flag(list(n_dm=n_dm,n_ae=n_ae)))
```

### XAGG-002 Visit count mismatch

-   Template T4
-   Axes A4 / group / programming_etl

``` r
v1 <- count_visits(VS, by=c("SITEID"))
v2 <- count_visits(LB, by=c("SITEID"))
bad <- compare(v1, v2, tol=1)
emit_group_flags(bad)
```

### XAGG-003 Domain completeness imbalance

-   Template T4
-   Axes A4 / group / ops_anomaly

``` r
comp <- completeness_by_site(domains=list(VS,LB,EG))
flag_outliers(comp)
```

### XAGG-004 Exposure vs event rate discrepancy

-   Template T4
-   Axes A4 / group / ops_anomaly

``` r
rate_ae <- ae_rate_by_site(AE)
rate_ex <- ex_completeness_by_site(EX)
bad <- rate_ae high & rate_ex low
emit_group_flags(bad)
```

------------------------------------------------------------------------

## A5 --- Cross-Snapshot Row (5)

### SNAP-001 Unexpected key deletions

-   Template T5
-   Axes A5 / record / programming_etl
-   Inputs: domain + IdentitySpec(keys)

``` r
removed <- anti_join(prev, cur, by=keys)
emit_row_flags(removed, message="Removed since last cut")
```

### SNAP-002 Record changed unexpectedly

-   Template T5
-   Axes A5 / record / programming_etl

``` r
aligned <- full_join(prev, cur, by=keys, suffix=c("_prev","_cur"))
bad <- any_field_changed(aligned, fields, tol=delta_tol)
emit_row_flags(bad)
```

### SNAP-003 Record reappeared

-   Template T5
-   Axes A5 / record / ops_anomaly

``` r
reappear <- keys_in(t2) that were removed in t1 then present again in t3
emit_row_flags(reappear)
```

### SNAP-004 Identity drift

-   Template T5
-   Axes A5 / record / programming_etl

``` r
# when "stable identity" is violated (e.g., AESEQ changes for same event)
bad <- detect_identity_instability(prev, cur, identity_fields)
emit_row_flags(bad)
```

### SNAP-005 Critical field modified post-lock

-   Template T5
-   Axes A5 / record / programming_etl

``` r
bad <- changed_fields(aligned, critical_fields)
emit_row_flags(bad, severity="error")
```

------------------------------------------------------------------------

## A6 --- Cross-Snapshot Aggregate (6)

### SNAP-006 Row count spike/drop

-   Template T6
-   Axes A6 / snapshot / ops_anomaly

``` r
n_cur <- nrow(cur); n_prev <- nrow(prev)
flag_if(abs(n_cur-n_prev) > thr, snapshot_flag(list(n_prev=n_prev,n_cur=n_cur)))
```

### SNAP-007 Key churn spike

-   Template T6
-   Axes A6 / snapshot / ops_anomaly

``` r
churn <- (new_keys + removed_keys) / keys_prev
flag_if(churn > thr, snapshot_flag(churn))
```

### SNAP-008 Missingness rate drift

-   Template T6
-   Axes A6 / group / ops_anomaly

``` r
miss_prev <- missing_rate(prev, group="SITEID")
miss_cur  <- missing_rate(cur, group="SITEID")
bad <- (miss_cur - miss_prev) > thr
emit_group_flags(bad)
```

### SNAP-009 Distribution drift

-   Template T6
-   Axes A6 / snapshot / ops_anomaly

``` r
drift <- ks_stat(prev[[x]], cur[[x]])
flag_if(drift > thr, snapshot_flag(drift))
```

### SNAP-010 Site metric drift

-   Template T6
-   Axes A6 / group / ops_anomaly

``` r
m_prev <- metric_by_site(prev)
m_cur  <- metric_by_site(cur)
emit_group_flags(abs(m_cur-m_prev) > thr)
```

### SNAP-011 Event rate change anomaly

-   Template T6
-   Axes A6 / group / ops_anomaly

``` r
rate_prev <- event_rate(prev, group="SITEID")
rate_cur  <- event_rate(cur, group="SITEID")
flag_outliers(rate_cur-rate_prev)
```

------------------------------------------------------------------------

## A7 --- Cross-Domain + Cross-Snapshot (4)

### BOTH-001 New AE without new EX

-   Template T7 (P1 diff→join)
-   Axes A7 / record / clinical_review
-   Inputs: AE, EX; IdentitySpec(AE); MatchSpec(AE↔EX)

``` r
new_ae <- anti_join(AE_cur, AE_prev, by=ae_keys)
bad <- anti_join(new_ae, EX_cur, by="USUBJID")
emit_row_flags(bad)
```

### BOTH-002 Dose changed + lab worsened

-   Template T7 (P3 metric→compare)
-   Axes A7 / group / clinical_review
-   Inputs: EX, LB; IdentitySpec(USUBJID)

``` r
dose_prev <- dose_metric(EX_prev)
dose_cur  <- dose_metric(EX_cur)
lab_prev  <- max_grade(LB_prev)
lab_cur   <- max_grade(LB_cur)
bad <- (dose_cur > dose_prev) & (lab_cur > lab_prev)
emit_group_flags(bad, entity="USUBJID")
```

### BOTH-003 Subject removed from DM but present elsewhere

-   Template T7 (P2 join→diff)
-   Axes A7 / record / programming_etl
-   Inputs: DM, AE (or any domain); IdentitySpec(DM)

``` r
removed_dm <- anti_join(DM_prev, DM_cur, by="USUBJID")
bad <- semi_join(removed_dm, AE_cur, by="USUBJID")
emit_row_flags(bad)
```

### BOTH-004 Risk score escalation trend

-   Template T7 (P3 metric→compare)
-   Axes A7 / group / ops_anomaly
-   Inputs: multiple domains; group=SITEID

``` r
score_prev <- compute_site_risk(prev_domains)
score_cur  <- compute_site_risk(cur_domains)
bad <- (score_cur - score_prev) > thr
emit_group_flags(bad, entity="SITEID")
```

------------------------------------------------------------------------

# Implementation Plan (how you build this without getting lost)

## Phase 1 --- Get the containers and registry right

1.  Finalize `DomainSnapshot`, `ArtifactSnapshot`, `RuleContext`
2.  Implement `Flag` + `FlagResult`
3.  Implement a registry: `register_rule()`, `get_rule()`,
    `pack_core_50()`

## Phase 2 --- Implement evaluators in order of leverage

1.  A1 (row predicates) --- unlocks many rules fast
2.  A2 (group aggregates) --- duplicates, rates, missingness
3.  A5 (snapshot row diffs) --- key deletions/changes
4.  A0 (schema signatures) --- schema drift
5.  A3 (cross-domain relational) --- integrity joins
6.  A6 (snapshot aggregate) --- churn, drift
7.  A4 (cross-domain aggregate) --- reconciliation
8.  A7 (cross-domain + snapshot) --- implement P1 and P2 first; P3 later

## Phase 3 --- Hardening

-   Deterministic ordering of flags
-   Snapshot provenance + hashes
-   Unit tests per topology using fixtures
-   Pack-level acceptance tests (run Core 50 on fixture snapshots)

------------------------------------------------------------------------

## Notes on naming and extensibility

-   IDs should be stable (don't renumber casually)
-   Axis tags should be immutable in registry (changing topology changes
    semantics)
-   Add new rules by choosing a template (T0--T7), not by inventing new
    topology

------------------------------------------------------------------------

*End of master plan.*
