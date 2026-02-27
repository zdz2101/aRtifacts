# Topologies A0-A7

[`run_rules()`](https://zdz2101.github.io/aRtifacts/reference/run_rules.md)
dispatches by `CoreRule@topology`.

| Topology | Payload to `fn`                        | Typical use                            |
|----------|----------------------------------------|----------------------------------------|
| A0       | `fn(domain, context, rule)`            | schema/snapshot drift                  |
| A1       | `fn(data, context, rule)`              | row-level single-domain checks         |
| A2       | `fn(data, context, rule)`              | grouped/aggregate single-domain checks |
| A3       | `fn(domains, context, rule)`           | cross-domain relational checks         |
| A4       | `fn(domains, context, rule)`           | cross-domain aggregate checks          |
| A5       | `fn(previous, current, context, rule)` | row-level longitudinal checks          |
| A6       | `fn(previous, current, context, rule)` | aggregate longitudinal checks          |
| A7       | `fn(previous, current, context, rule)` | cross-domain longitudinal checks       |

## Topology chooser (yes/no flow)

Use this when you are writing a new rule and need the right topology
quickly.

``` text
Start
|
+-- Compare current snapshot to previous snapshot?
    |
    +-- No (single-cut rule)
    |   |
    |   +-- Uses more than one domain?
    |       |
    |       +-- No (single-domain)
    |       |   |
    |       |   +-- Structural/schema-only check? -> A0
    |       |   +-- Row-level record check?       -> A1
    |       |   +-- Group/aggregate check?        -> A2
    |       |
    |       +-- Yes (cross-domain)
    |           |
    |           +-- Join/relationship integrity?  -> A3
    |           +-- Cross-domain aggregates/rates?-> A4
    |
    +-- Yes (longitudinal rule)
        |
        +-- Uses more than one domain?
            |
            +-- No (single-domain across cuts)
            |   |
            |   +-- Row/key/value drift check?    -> A5
            |   +-- Aggregate metric drift check? -> A6
            |
            +-- Yes (cross-domain across cuts)    -> A7
```

If a rule can fit multiple choices, pick the smallest payload that
works.

You can also map the same decision in code:

``` r
choose_topology(is_longitudinal = FALSE, n_domains = 2, level = "relational")
#> [1] "A3"
choose_topology(is_longitudinal = TRUE, n_domains = 1, level = "aggregate")
#> [1] "A6"
```

## Starter packs

``` r
pack_topology_a0(domain = "PRIMARY")
pack_topology_a1(domain = "PRIMARY", key_vars = c("entity_id", "record_id"))
pack_topology_a2(domain = "PRIMARY", key_vars = c("entity_id", "record_id"))
pack_topology_a3()
pack_topology_a4()
pack_topology_a5(domain_primary = "PRIMARY", keys = c("entity_id", "record_id"))
pack_topology_a6(domain_primary = "PRIMARY", keys = c("entity_id", "record_id"))
pack_topology_a7()
```

Each `pack_topology_a*()` function returns a list of `CoreRule` objects.
Use `register_topology_a*()` if you want those rules placed in the
global registry.

## How to apply a prebuilt pack

1.  Pick topology with the yes/no flow (or
    [`choose_topology()`](https://zdz2101.github.io/aRtifacts/reference/choose_topology.md)).
2.  Map your artifact domain names to the pack arguments.
3.  Build the correct rule context:
    - single-cut (`A0`-`A4`):
      `new_rule_context(current = artifact_current)`
    - longitudinal (`A5`-`A7`):
      `new_rule_context(current = artifact_current, previous = artifact_previous)`
4.  Run `run_rules(context, pack_topology_a*)`.

## Minimum context by pack

| Pack                                                                                      | Needs `previous` artifact? | Minimum domains expected in `current`                                                                                                 | Key args to map first                              |
|-------------------------------------------------------------------------------------------|----------------------------|---------------------------------------------------------------------------------------------------------------------------------------|----------------------------------------------------|
| [`pack_topology_a0()`](https://zdz2101.github.io/aRtifacts/reference/pack_topology_a0.md) | No                         | `domain`                                                                                                                              | `domain`                                           |
| [`pack_topology_a1()`](https://zdz2101.github.io/aRtifacts/reference/pack_topology_a1.md) | No                         | `domain`                                                                                                                              | `domain`, `key_vars`                               |
| [`pack_topology_a2()`](https://zdz2101.github.io/aRtifacts/reference/pack_topology_a2.md) | No                         | `domain`                                                                                                                              | `domain`, `key_vars`, `site_var`, `subject_var`    |
| [`pack_topology_a3()`](https://zdz2101.github.io/aRtifacts/reference/pack_topology_a3.md) | No                         | `primary_domain`, `reference_domain`, `child_domain`, `parent_domain`, `left_domain`, `right_domain`, `event_domain`, `window_domain` | domain-name args and `link_keys`/`parent_keys`     |
| [`pack_topology_a4()`](https://zdz2101.github.io/aRtifacts/reference/pack_topology_a4.md) | No                         | `left_domain`, `right_domain`, all `completeness_domains`, `event_domain`, `support_domain`                                           | domain-name args, `entity_var`, `group_var`        |
| [`pack_topology_a5()`](https://zdz2101.github.io/aRtifacts/reference/pack_topology_a5.md) | Yes                        | `domain_primary` in both `current` and `previous`                                                                                     | `domain_primary`, `keys`, `monitored_fields`       |
| [`pack_topology_a6()`](https://zdz2101.github.io/aRtifacts/reference/pack_topology_a6.md) | Yes                        | `domain_primary` in both `current` and `previous`                                                                                     | `domain_primary`, `keys`, `group_var`, `value_var` |
| [`pack_topology_a7()`](https://zdz2101.github.io/aRtifacts/reference/pack_topology_a7.md) | Yes                        | `primary_domain`, `support_domain`, `other_domain`, `driver_domain`, `metric_domain`, `events_domain` in both cuts                    | domain-name args plus `primary_keys`/`match_keys`  |

## Prebuilt pack recipes (A0-A7)

### A0: schema/snapshot checks

``` r
rules_a0 <- pack_topology_a0(domain = "PRIMARY")
ctx <- new_rule_context(current = artifact_current)
run_rules(ctx, rules_a0)
```

### A1: single-domain row-level checks

``` r
rules_a1 <- pack_topology_a1(
  domain = "PRIMARY",
  key_vars = c("entity_id", "record_id")
)
ctx <- new_rule_context(current = artifact_current)
run_rules(ctx, rules_a1)
```

### A2: single-domain aggregate checks

``` r
rules_a2 <- pack_topology_a2(
  domain = "PRIMARY",
  key_vars = c("entity_id", "record_id"),
  site_var = "group_id",
  subject_var = "entity_id"
)
ctx <- new_rule_context(current = artifact_current)
run_rules(ctx, rules_a2)
```

### A3: cross-domain relational checks

``` r
rules_a3 <- pack_topology_a3(
  primary_domain = "PRIMARY",
  reference_domain = "REFERENCE",
  child_domain = "CHILD",
  parent_domain = "PARENT",
  left_domain = "LEFT",
  right_domain = "RIGHT",
  event_domain = "EVENTS",
  window_domain = "WINDOW",
  link_keys = c("entity_id"),
  parent_keys = c("entity_id")
)
ctx <- new_rule_context(current = artifact_current)
run_rules(ctx, rules_a3)
```

### A4: cross-domain aggregate checks

``` r
rules_a4 <- pack_topology_a4(
  left_domain = "LEFT",
  right_domain = "RIGHT",
  completeness_domains = c("PRIMARY", "SUPPORT", "AUX"),
  event_domain = "EVENTS",
  support_domain = "SUPPORT",
  entity_var = "entity_id",
  group_var = "group_id"
)
ctx <- new_rule_context(current = artifact_current)
run_rules(ctx, rules_a4)
```

### A5: single-domain longitudinal row/value drift checks

``` r
rules_a5 <- pack_topology_a5(
  domain_primary = "PRIMARY",
  keys = c("entity_id", "record_id"),
  monitored_fields = c("value", "status_flag")
)
ctx <- new_rule_context(current = artifact_current, previous = artifact_previous)
run_rules(ctx, rules_a5)
```

### A6: single-domain longitudinal aggregate drift checks

``` r
rules_a6 <- pack_topology_a6(
  domain_primary = "PRIMARY",
  keys = c("entity_id", "record_id"),
  group_var = "group_id",
  value_var = "value"
)
ctx <- new_rule_context(current = artifact_current, previous = artifact_previous)
run_rules(ctx, rules_a6)
```

### A7: cross-domain longitudinal checks

``` r
rules_a7 <- pack_topology_a7(
  primary_domain = "PRIMARY",
  support_domain = "SUPPORT",
  other_domain = "OTHER",
  driver_domain = "DRIVER",
  metric_domain = "METRIC",
  events_domain = "EVENTS",
  primary_keys = c("entity_id", "record_id"),
  match_keys = c("entity_id")
)
ctx <- new_rule_context(current = artifact_current, previous = artifact_previous)
run_rules(ctx, rules_a7)
```

## Runnable `data.frame()` demos with intentional violations

These examples use plain
[`data.frame()`](https://rdrr.io/r/base/data.frame.html) fixtures and
run prebuilt packs end-to-end. Each demo is set up to trigger at least
one clear finding.

``` r
mk_domain <- function(name, data, key_vars) {
  new_domain_snapshot(name = name, data = data, key_vars = key_vars)
}

mk_artifact <- function(domains, build_id) {
  new_artifact_snapshot(domains = domains, build_id = build_id)
}
```

### A0 demo: schema addition (`SCH-001`)

``` r
prev_inventory <- data.frame(
  record_key = 1:2,
  product_id = c("P100", "P200"),
  quantity = c(4, 9),
  stringsAsFactors = FALSE
)
cur_inventory <- data.frame(
  record_key = 1:2,
  product_id = c("P100", "P200"),
  quantity = c(4, 9),
  warehouse_zone = c("A1", "B2"),
  stringsAsFactors = FALSE
)

ctx_a0 <- new_rule_context(
  previous = mk_artifact(list(INVENTORY = mk_domain("INVENTORY", prev_inventory, "record_key")), build_id = "v1"),
  current = mk_artifact(list(INVENTORY = mk_domain("INVENTORY", cur_inventory, "record_key")), build_id = "v2")
)

run_rules(ctx_a0, pack_topology_a0(domain = "INVENTORY"))
#>   domain_name         column change RULE_ID        RULE_TITLE TOPOLOGY    SCOPE
#> 1   INVENTORY warehouse_zone  added SCH-001 New columns added       A0 snapshot
#>            INTENT SEVERITY OWNER_TEAM
#> 1 programming_etl  warning   Biostats
```

### A1 demo: missing key fields (`ID-001`)

``` r
primary_a1 <- data.frame(
  row_id = 1:3,
  entity_id = c("E1", "", "E3"),
  record_id = c(101, 102, NA),
  stringsAsFactors = FALSE
)

ctx_a1 <- new_rule_context(
  current = mk_artifact(list(PRIMARY = mk_domain("PRIMARY", primary_a1, "row_id")), build_id = "v1")
)

run_rules(
  ctx_a1,
  pack_topology_a1(
    domain = "PRIMARY",
    key_vars = c("entity_id", "record_id")
  )
)
#>   entity_id record_id row_id                  reason RULE_ID         RULE_TITLE
#> 2                 102      2 Missing key field value  ID-001 Missing key fields
#> 3        E3        NA      3 Missing key field value  ID-001 Missing key fields
#>   TOPOLOGY  SCOPE         INTENT SEVERITY       OWNER_TEAM
#> 2       A1 record core_integrity  warning Data Stewardship
#> 3       A1 record core_integrity  warning Data Stewardship
```

### A2 demo: duplicate key combinations (`ID-002`)

``` r
primary_a2 <- data.frame(
  row_id = 1:4,
  entity_id = c("E1", "E1", "E2", "E3"),
  record_id = c(1, 1, 2, 3),
  stringsAsFactors = FALSE
)

ctx_a2 <- new_rule_context(
  current = mk_artifact(list(PRIMARY = mk_domain("PRIMARY", primary_a2, "row_id")), build_id = "v1")
)

run_rules(
  ctx_a2,
  pack_topology_a2(
    domain = "PRIMARY",
    key_vars = c("entity_id", "record_id"),
    site_var = "site_id",
    subject_var = "subject_id",
    max_events = 99
  )
)
#>   entity_id record_id row_id                    reason RULE_ID     RULE_TITLE
#> 1        E1         1      1 Duplicate key combination  ID-002 Duplicate keys
#> 2        E1         1      2 Duplicate key combination  ID-002 Duplicate keys
#>   TOPOLOGY SCOPE         INTENT SEVERITY       OWNER_TEAM
#> 1       A2 group core_integrity  warning Data Stewardship
#> 2       A2 group core_integrity  warning Data Stewardship
```

### A3 demo: missing cross-domain references (`REL-001`)

``` r
primary_a3 <- data.frame(entity_id = c("E1", "E2", "E3"), stringsAsFactors = FALSE)
reference_a3 <- data.frame(entity_id = c("E1", "E2"), stringsAsFactors = FALSE)

ctx_a3 <- new_rule_context(
  current = mk_artifact(
    list(
      PRIMARY = mk_domain("PRIMARY", primary_a3, "entity_id"),
      REFERENCE = mk_domain("REFERENCE", reference_a3, "entity_id")
    ),
    build_id = "v1"
  )
)

run_rules(
  ctx_a3,
  pack_topology_a3(
    primary_domain = "PRIMARY",
    reference_domain = "REFERENCE",
    link_keys = "entity_id"
  )
)
#>   entity_id primary_domain reference_domain
#> 3        E3        PRIMARY        REFERENCE
#>                                               reason RULE_ID
#> 3 Primary-domain key not present in reference domain REL-001
#>                  RULE_TITLE TOPOLOGY  SCOPE         INTENT SEVERITY
#> 3 Missing reference records       A3 record core_integrity  warning
#>         OWNER_TEAM
#> 3 Data Stewardship
```

### A4 demo: entity-count mismatch (`AGG-001`)

``` r
left_a4 <- data.frame(entity_id = c("E1", "E2", "E3"), stringsAsFactors = FALSE)
right_a4 <- data.frame(entity_id = c("E1", "E2"), stringsAsFactors = FALSE)

ctx_a4 <- new_rule_context(
  current = mk_artifact(
    list(
      LEFT = mk_domain("LEFT", left_a4, "entity_id"),
      RIGHT = mk_domain("RIGHT", right_a4, "entity_id")
    ),
    build_id = "v1"
  )
)

run_rules(
  ctx_a4,
  pack_topology_a4(
    left_domain = "LEFT",
    right_domain = "RIGHT",
    entity_var = "entity_id"
  )
)
#>   left_domain right_domain entity_var n_left n_right delta_abs threshold
#> 1        LEFT        RIGHT  entity_id      3       2         1         0
#>                                           reason RULE_ID
#> 1 Distinct entity counts differ beyond tolerance AGG-001
#>                       RULE_TITLE TOPOLOGY    SCOPE         INTENT SEVERITY
#> 1 Distinct entity count mismatch       A4 snapshot core_integrity  warning
#>        OWNER_TEAM
#> 1 Data Operations
```

### A5 demo: unexpected deletions across cuts (`SNAP-001`)

``` r
prev_a5 <- data.frame(entity_id = c("E1", "E2", "E3"), stringsAsFactors = FALSE)
cur_a5 <- data.frame(entity_id = c("E1", "E2"), stringsAsFactors = FALSE)

ctx_a5 <- new_rule_context(
  previous = mk_artifact(list(PRIMARY = mk_domain("PRIMARY", prev_a5, "entity_id")), build_id = "v1"),
  current = mk_artifact(list(PRIMARY = mk_domain("PRIMARY", cur_a5, "entity_id")), build_id = "v2")
)

run_rules(
  ctx_a5,
  pack_topology_a5(
    domain_primary = "PRIMARY",
    keys = "entity_id"
  )
)
#>   entity_id                 reason  RULE_ID               RULE_TITLE TOPOLOGY
#> 3        E3 Removed since last cut SNAP-001 Unexpected key deletions       A5
#>    SCOPE          INTENT SEVERITY       OWNER_TEAM
#> 3 record programming_etl  warning Data Stewardship
```

### A6 demo: row-count spike across cuts (`SNAP-006`)

``` r
prev_a6 <- data.frame(entity_id = sprintf("E%02d", 1:10), stringsAsFactors = FALSE)
cur_a6 <- data.frame(entity_id = sprintf("E%02d", 1:12), stringsAsFactors = FALSE)

ctx_a6 <- new_rule_context(
  previous = mk_artifact(list(PRIMARY = mk_domain("PRIMARY", prev_a6, "entity_id")), build_id = "v1"),
  current = mk_artifact(list(PRIMARY = mk_domain("PRIMARY", cur_a6, "entity_id")), build_id = "v2")
)

run_rules(
  ctx_a6,
  pack_topology_a6(
    domain_primary = "PRIMARY",
    keys = "entity_id",
    threshold_row_abs = 1,
    threshold_row_rel = 0.1,
    churn_threshold = 0.9,
    group_var = "group_id",
    value_var = "value"
  )
)
#>   n_prev n_cur delta_n delta_abs delta_rel threshold_abs threshold_rel
#> 1     10    12       2         2       0.2             1           0.1
#>                               reason  RULE_ID           RULE_TITLE TOPOLOGY
#> 1 Row count changed beyond threshold SNAP-006 Row count spike/drop       A6
#>      SCOPE      INTENT SEVERITY      OWNER_TEAM
#> 1 snapshot ops_anomaly  warning Data Operations
```

### A7 demo: new primary without support (`HYB-001`)

``` r
primary_prev <- data.frame(
  entity_id = c("E1", "E2"),
  record_id = c(1, 1),
  stringsAsFactors = FALSE
)
primary_cur <- data.frame(
  entity_id = c("E1", "E2", "E3"),
  record_id = c(1, 1, 1),
  stringsAsFactors = FALSE
)
support_prev <- data.frame(entity_id = c("E1", "E2"), stringsAsFactors = FALSE)
support_cur <- data.frame(entity_id = c("E1", "E2"), stringsAsFactors = FALSE)

ctx_a7 <- new_rule_context(
  previous = mk_artifact(
    list(
      PRIMARY = mk_domain("PRIMARY", primary_prev, c("entity_id", "record_id")),
      SUPPORT = mk_domain("SUPPORT", support_prev, "entity_id")
    ),
    build_id = "v1"
  ),
  current = mk_artifact(
    list(
      PRIMARY = mk_domain("PRIMARY", primary_cur, c("entity_id", "record_id")),
      SUPPORT = mk_domain("SUPPORT", support_cur, "entity_id")
    ),
    build_id = "v2"
  )
)

run_rules(
  ctx_a7,
  pack_topology_a7(
    primary_domain = "PRIMARY",
    support_domain = "SUPPORT",
    primary_keys = c("entity_id", "record_id"),
    match_keys = "entity_id"
  )
)
#>   entity_id record_id primary_domain support_domain
#> 3        E3         1        PRIMARY        SUPPORT
#>                                              reason RULE_ID
#> 3 New primary record has no matching support record HYB-001
#>                                RULE_TITLE TOPOLOGY  SCOPE      INTENT SEVERITY
#> 3 New primary records without new support       A7 record ops_anomaly  warning
#>        OWNER_TEAM
#> 3 Data Operations
```

## Dispatch notes for developers

1.  A3/A4/A7 rules must declare `domains` in
    [`new_rule()`](https://zdz2101.github.io/aRtifacts/reference/new_rule.md).
2.  A5/A6 rules must declare `domain` and require `context@previous`.
3.  Missing required domains cause rule skip (empty output), not
    failure.
4.  Every non-empty rule output is augmented with standard metadata
    columns.
