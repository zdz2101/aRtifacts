#' Topology starter rule catalog
#'
#' Returns metadata for all bundled topology starter rules in core.
#'
#' @return A data frame with columns `rule_id`, `rule_name`, `topology`,
#'   `scope`, `intent`, and `category`.
#' @export
rule_catalog_core <- function() {
  rules <- c(
    pack_topology_a0(domain = "PRIMARY"),
    pack_topology_a1(domain = "PRIMARY", key_vars = c("entity_id")),
    pack_topology_a2(domain = "PRIMARY", key_vars = c("entity_id")),
    pack_topology_a3(),
    pack_topology_a4(),
    pack_topology_a5(domain_primary = "PRIMARY", keys = c("entity_id")),
    pack_topology_a6(domain_primary = "PRIMARY", keys = c("entity_id")),
    pack_topology_a7()
  )

  if (!length(rules)) {
    return(data.frame(
      rule_id = character(),
      rule_name = character(),
      topology = character(),
      scope = character(),
      intent = character(),
      category = character(),
      stringsAsFactors = FALSE
    ))
  }

  to_category <- function(intent) {
    gsub("_", "-", tools::toTitleCase(intent))
  }

  out <- lapply(rules, function(rule) {
    data.frame(
      rule_id = rule@id,
      rule_name = rule@title,
      topology = rule@topology,
      scope = tools::toTitleCase(rule@scope),
      intent = rule@intent,
      category = to_category(rule@intent),
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, out)
  rownames(out) <- NULL
  out[order(out$topology, out$rule_id), , drop = FALSE]
}
