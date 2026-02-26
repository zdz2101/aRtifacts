# ---------------------------------------------------------------------------
# Minimal topology rule template: A5 (single-domain longitudinal row-level)
# Payload to fn: (previous, current, context, rule)
# ---------------------------------------------------------------------------

make_rule_template_a5 <- function(domain = "PRIMARY", owner_team = "Data Operations") {
  new_rule(
    id = "CUST-A5-001",
    title = "Longitudinal row-level drift check",
    topology = "A5",
    scope = "record",
    intent = "ops_anomaly",
    domain = domain,
    owner_team = owner_team,
    fn = function(previous, current, context, rule) {
      # TODO: compare previous/current row-level behavior.
      data.frame()
    }
  )
}
