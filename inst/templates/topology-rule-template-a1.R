# ---------------------------------------------------------------------------
# Minimal topology rule template: A1 (single-domain row-level)
# Payload to fn: (data, context, rule)
# ---------------------------------------------------------------------------

make_rule_template_a1 <- function(domain = "PRIMARY", owner_team = "Data Quality") {
  new_rule(
    id = "CUST-A1-001",
    title = "Row-level check",
    topology = "A1",
    scope = "record",
    intent = "quality_screening",
    domain = domain,
    owner_team = owner_team,
    fn = function(data, context, rule) {
      # TODO: return flagged rows from `data`.
      data.frame()
    }
  )
}
