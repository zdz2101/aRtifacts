# Add a Rule Pack

A rule pack is a function that returns a list of `CoreRule` objects.

Before writing a new rule, pick topology via the yes/no flow in vignette
**Topologies A0-A7** (`topology-a0-a7`). You can also resolve topology
in code with
[`choose_topology()`](https://zdz2101.github.io/aRtifacts/reference/choose_topology.md).

``` r
library(aRtifacts)

make_rule_pack_primary <- function(domain = "PRIMARY", owner_team = "Data Stewardship", max_value = 50) {
  list(
    new_rule(
      id = "CUST-001",
      title = "Value over limit",
      topology = "A1",
      scope = "record",
      intent = "ops_anomaly",
      domain = domain,
      owner_team = owner_team,
      fn = function(data, context, rule) {
        if (!all(c("entity_id", "record_id", "value") %in% names(data))) return(data.frame())
        data[data$value > max_value, c("entity_id", "record_id", "value"), drop = FALSE]
      }
    )
  )
}
```

``` r
primary <- data.frame(
  entity_id = c("E001", "E002", "E003"),
  record_id = c(1, 2, 3),
  value = c(10, 60, 30),
  stringsAsFactors = FALSE
)

snap <- new_domain_snapshot("PRIMARY", primary, key_vars = c("entity_id", "record_id"))
art <- new_artifact_snapshot(domains = list(PRIMARY = snap), build_id = "cut-001")
ctx <- new_rule_context(current = art)

run_rules(ctx, make_rule_pack_primary())
#>   entity_id record_id value  RULE_ID       RULE_TITLE TOPOLOGY  SCOPE
#> 2      E002         2    60 CUST-001 Value over limit       A1 record
#>        INTENT SEVERITY       OWNER_TEAM
#> 2 ops_anomaly  warning Data Stewardship
```

For package developers, templates are available in `inst/templates/`.

## Rule Builder Funnel App (Preview + Scaffold)

For repeatable pack setup and topology capture, launch the in-package
app:

``` r
launch_rule_builder(output_dir = getwd(), dry_run = TRUE)
```

The app opens in a **Guided Builder** one-way funnel:

1.  Topology questions.
2.  Core metadata and domain capture.
3.  Confirm and add rule.
4.  Repeat (`Add Another Rule`) or finish (`Finish Pack`).

An **Advanced Workspace** tab is available for full edit/reorder
workflows.

On confirmed export, files are written to a timestamped folder (no
overwrite in place):

- `rule-pack.spec.yaml`
- `rule_pack_<slug>.R`
- `agent_prompt_rule_pack_<slug>.md`
