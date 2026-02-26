# ---------------------------------------------------------------------------
# Minimal topology rule template: A2 (single-domain aggregate)
# Payload to fn: (data, context, rule)
# ---------------------------------------------------------------------------

make_rule_template_a2 <- function(domain = "PRIMARY", owner_team = "Data Quality") {
  new_rule(
    id = "CUST-A2-001",
    title = "Aggregate check",
    topology = "A2",
    scope = "group",
    intent = "quality_screening",
    domain = domain,
    owner_team = owner_team,
    fn = function(data, context, rule) {
      # TODO: compute grouped metrics and return violating groups.
      data.frame()
    }
  )
}
