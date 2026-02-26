# ---------------------------------------------------------------------------
# Minimal topology rule template: A6 (single-domain longitudinal aggregate)
# Payload to fn: (previous, current, context, rule)
# ---------------------------------------------------------------------------

make_rule_template_a6 <- function(domain = "PRIMARY", owner_team = "Data Operations") {
  new_rule(
    id = "CUST-A6-001",
    title = "Longitudinal aggregate drift check",
    topology = "A6",
    scope = "group",
    intent = "ops_anomaly",
    domain = domain,
    owner_team = owner_team,
    fn = function(previous, current, context, rule) {
      # TODO: compare previous/current aggregate metrics.
      data.frame()
    }
  )
}
