# ---------------------------------------------------------------------------
# Minimal topology rule template: A0 (schema / domain contract)
# Payload to fn: (domain, context, rule)
# ---------------------------------------------------------------------------

make_rule_template_a0 <- function(domain = "PRIMARY", owner_team = "Data Quality") {
  new_rule(
    id = "CUST-A0-001",
    title = "Schema contract check",
    topology = "A0",
    scope = "snapshot",
    intent = "schema_contract",
    domain = domain,
    owner_team = owner_team,
    fn = function(domain, context, rule) {
      df <- domain@data
      # TODO: return findings data.frame with context columns.
      data.frame()
    }
  )
}
