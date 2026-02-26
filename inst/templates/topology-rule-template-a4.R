# ---------------------------------------------------------------------------
# Minimal topology rule template: A4 (cross-domain aggregate)
# Payload to fn: (domains, context, rule)
# ---------------------------------------------------------------------------

make_rule_template_a4 <- function(left_domain = "LEFT",
                                  right_domain = "RIGHT",
                                  owner_team = "Data Stewardship") {
  new_rule(
    id = "CUST-A4-001",
    title = "Cross-domain aggregate check",
    topology = "A4",
    scope = "snapshot",
    intent = "core_integrity",
    domains = c(left_domain, right_domain),
    owner_team = owner_team,
    fn = function(domains, context, rule) {
      left <- domains[[left_domain]]
      right <- domains[[right_domain]]
      # TODO: aggregate reconciliation and findings output.
      data.frame()
    }
  )
}
