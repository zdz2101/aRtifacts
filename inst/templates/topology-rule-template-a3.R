# ---------------------------------------------------------------------------
# Minimal topology rule template: A3 (cross-domain relational)
# Payload to fn: (domains, context, rule)
# ---------------------------------------------------------------------------

make_rule_template_a3 <- function(left_domain = "LEFT",
                                  right_domain = "RIGHT",
                                  owner_team = "Data Stewardship") {
  new_rule(
    id = "CUST-A3-001",
    title = "Cross-domain relational check",
    topology = "A3",
    scope = "record",
    intent = "core_integrity",
    domains = c(left_domain, right_domain),
    owner_team = owner_team,
    fn = function(domains, context, rule) {
      left <- domains[[left_domain]]
      right <- domains[[right_domain]]
      # TODO: join/relationship validation and findings output.
      data.frame()
    }
  )
}
