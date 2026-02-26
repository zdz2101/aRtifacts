# ---------------------------------------------------------------------------
# Minimal topology rule template: A7 (cross-domain longitudinal)
# Payload to fn: (previous, current, context, rule)
# ---------------------------------------------------------------------------

make_rule_template_a7 <- function(primary_domain = "PRIMARY",
                                  support_domain = "SUPPORT",
                                  owner_team = "Data Operations") {
  new_rule(
    id = "CUST-A7-001",
    title = "Cross-domain longitudinal check",
    topology = "A7",
    scope = "record",
    intent = "ops_anomaly",
    domains = c(primary_domain, support_domain),
    owner_team = owner_team,
    fn = function(previous, current, context, rule) {
      prev_primary <- previous[[primary_domain]]
      curr_primary <- current[[primary_domain]]
      curr_support <- current[[support_domain]]
      # TODO: evaluate cross-domain behavior across cuts.
      data.frame()
    }
  )
}
