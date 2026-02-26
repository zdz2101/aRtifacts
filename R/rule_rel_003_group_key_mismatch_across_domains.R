#' REL-003: Group-key mismatch across domains
#'
#' @param left_domain Left domain name.
#' @param right_domain Right domain name.
#' @param entity_var Entity identifier variable.
#' @param left_group_var Group variable in left domain.
#' @param right_group_var Group variable in right domain.
#' @param owner_team Owner team.
#'
#' @return A `CoreRule` object.
#' @export
rule_rel_003_group_key_mismatch_across_domains <- function(left_domain = "LEFT",
                                                           right_domain = "RIGHT",
                                                           entity_var = "entity_id",
                                                           left_group_var = "group_id",
                                                           right_group_var = "group_id",
                                                           owner_team = "Data Stewardship") {
  new_rule(
    id = "REL-003",
    title = "Group-key mismatch across domains",
    topology = "A3",
    scope = "record",
    intent = "programming_etl",
    domains = c(left_domain, right_domain),
    owner_team = owner_team,
    fn = function(domains, context, rule) {
      left <- .topology_domain_df(domains, left_domain)
      right <- .topology_domain_df(domains, right_domain)
      if (is.null(left) || is.null(right)) {
        return(data.frame())
      }

      needed_left <- c(entity_var, left_group_var)
      needed_right <- c(entity_var, right_group_var)
      if (!all(needed_left %in% names(left)) || !all(needed_right %in% names(right))) {
        return(data.frame())
      }

      left_key <- unique(data.frame(
        entity_id = as.character(left[[entity_var]]),
        group_id = as.character(left[[left_group_var]]),
        stringsAsFactors = FALSE
      ))
      right_key <- unique(data.frame(
        entity_id = as.character(right[[entity_var]]),
        group_id = as.character(right[[right_group_var]]),
        stringsAsFactors = FALSE
      ))

      only_left <- .topology_anti_join_rows(left_key, right_key, by = c("entity_id", "group_id"))
      only_right <- .topology_anti_join_rows(right_key, left_key, by = c("entity_id", "group_id"))
      if (!nrow(only_left) && !nrow(only_right)) {
        return(data.frame())
      }

      out <- data.frame()
      if (nrow(only_left)) {
        only_left$location <- "left_only"
        out <- rbind(out, only_left)
      }
      if (nrow(only_right)) {
        only_right$location <- "right_only"
        out <- rbind(out, only_right)
      }

      out$left_domain <- left_domain
      out$right_domain <- right_domain
      out$reason <- "Entity/group pair appears in only one domain"
      out
    }
  )
}
