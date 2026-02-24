# ---------------------------------------------------------------------------
# Module 5 — Cross-Field & Relational Consistency (R040–R048)
#
# R042 (derived flag inconsistent with source) requires user-supplied
# validation logic that is too domain-specific to generalize here — callers
# should implement it directly as a custom FlagRule.
# R044 (controlled terminology drift) requires an external CT reference
# table and is not implementable without it.
# R046 (child record without parent) requires cross-domain data that is not
# available inside a single-domain rule pack.
# R049 (enrollment vs assessment mismatch) and R050 (record deletions) are
# Snapshot-scope and belong in the diff engine.
# All are noted in the catalog but not implemented here.
# ---------------------------------------------------------------------------

#' Rule pack — cross-field and relational consistency
#'
#' Builds executable `FlagRule` objects for the cross-field and relational
#' consistency checks from the core observability rule catalog.
#'
#' | Rule ID | Name | Scope |
#' |---|---|---|
#' | R040 | Impossible categorical combinations | Record |
#' | R041 | Inconsistent categorical pairs | Record |
#' | R043 | Placeholder text prevalence spike | Record |
#' | R045 | Unexpected explosion in distinct categorical values | Snapshot |
#' | R047 | Sequence gaps or duplicate sequence numbers | Group |
#' | R048 | Record count per subject outlier | Group |
#'
#' @param domain Domain name targeted by the rules.
#' @param key_vars Character vector of key variables.
#' @param subject_var Subject identifier variable. Default `"USUBJID"`.
#' @param group_var Group identifier variable. Default `"SITEID"`.
#' @param impossible_combos List of named character vectors that define
#'   forbidden combinations. Each element should be a named vector where
#'   names are column names and values are the forbidden values that must
#'   not appear together, e.g.
#'   `list(c(SEX = "M", PREGNANT = "Y"))`.
#' @param inconsistent_pairs List of named character vectors specifying
#'   values that are mutually exclusive within the same record, e.g.
#'   `list(c(AESER = "Y", AESDTH = "N", AESLIFE = "N", AESHOSP = "N",
#'            AESDISAB = "N", AESCONG = "N", AESMIE = "N"))`.
#'   A record is flagged when *all* listed values match simultaneously.
#' @param categorical_vars Character variables to inspect for unusual
#'   cardinality (R045).
#' @param seq_vars Character vector of integer sequence variables to check
#'   for gaps or duplicates within each subject (R047).
#' @param placeholder_patterns Character vector of regex patterns that
#'   identify placeholder text (e.g. `"^N/A$"`, `"^TBD$"`). Defaults to a
#'   built-in set covering common data-entry placeholders.
#' @param max_distinct_values Maximum number of distinct values allowed per
#'   categorical variable before R045 fires. Default `50L`.
#' @param record_count_iqr_multiplier IQR multiplier for the per-subject
#'   record count outlier check (R048). Default `3.0`.
#' @param owner_team Team assigned to follow up on flagged records.
#' @param ... Reserved for future extensions; currently ignored.
#'
#' @return A list of `FlagRule` objects compatible with `run_flags()`.
#' @export
#'
#' @examples
#' pack <- make_rule_pack_consistency(
#'   domain            = "ADVS",
#'   key_vars          = c("USUBJID", "PARAMCD", "AVISIT"),
#'   categorical_vars  = c("PARAMCD", "AVISIT"),
#'   seq_vars          = character()
#' )
#' length(pack)
make_rule_pack_consistency <- function(domain,
                                       key_vars,
                                       subject_var              = "USUBJID",
                                       group_var                 = "SITEID",
                                       impossible_combos        = list(),
                                       inconsistent_pairs       = list(),
                                       categorical_vars         = character(),
                                       seq_vars                 = character(),
                                       placeholder_patterns     = NULL,
                                       max_distinct_values      = 50L,
                                       record_count_iqr_multiplier = 3.0,
                                       owner_team               = "Standards",
                                       ...) {
  rules <- list(
    R040 = rule_r040_impossible_combos(domain = domain, key_vars = key_vars, subject_var = subject_var, group_var = group_var, impossible_combos = impossible_combos, inconsistent_pairs = inconsistent_pairs, categorical_vars = categorical_vars, seq_vars = seq_vars, placeholder_patterns = placeholder_patterns, max_distinct_values = max_distinct_values, record_count_iqr_multiplier = record_count_iqr_multiplier, owner_team = owner_team, ...),
    R041 = rule_r041_inconsistent_pairs(domain = domain, key_vars = key_vars, subject_var = subject_var, group_var = group_var, impossible_combos = impossible_combos, inconsistent_pairs = inconsistent_pairs, categorical_vars = categorical_vars, seq_vars = seq_vars, placeholder_patterns = placeholder_patterns, max_distinct_values = max_distinct_values, record_count_iqr_multiplier = record_count_iqr_multiplier, owner_team = owner_team, ...),
    R042 = rule_r042_derived_inconsistent(domain = domain, key_vars = key_vars, subject_var = subject_var, group_var = group_var, impossible_combos = impossible_combos, inconsistent_pairs = inconsistent_pairs, categorical_vars = categorical_vars, seq_vars = seq_vars, placeholder_patterns = placeholder_patterns, max_distinct_values = max_distinct_values, record_count_iqr_multiplier = record_count_iqr_multiplier, owner_team = owner_team, ...),
    R043 = rule_r043_placeholder_text(domain = domain, key_vars = key_vars, subject_var = subject_var, group_var = group_var, impossible_combos = impossible_combos, inconsistent_pairs = inconsistent_pairs, categorical_vars = categorical_vars, seq_vars = seq_vars, placeholder_patterns = placeholder_patterns, max_distinct_values = max_distinct_values, record_count_iqr_multiplier = record_count_iqr_multiplier, owner_team = owner_team, ...),
    R044 = rule_r044_ct_drift(domain = domain, key_vars = key_vars, subject_var = subject_var, group_var = group_var, impossible_combos = impossible_combos, inconsistent_pairs = inconsistent_pairs, categorical_vars = categorical_vars, seq_vars = seq_vars, placeholder_patterns = placeholder_patterns, max_distinct_values = max_distinct_values, record_count_iqr_multiplier = record_count_iqr_multiplier, owner_team = owner_team, ...),
    R045 = rule_r045_distinct_values_explosion(domain = domain, key_vars = key_vars, subject_var = subject_var, group_var = group_var, impossible_combos = impossible_combos, inconsistent_pairs = inconsistent_pairs, categorical_vars = categorical_vars, seq_vars = seq_vars, placeholder_patterns = placeholder_patterns, max_distinct_values = max_distinct_values, record_count_iqr_multiplier = record_count_iqr_multiplier, owner_team = owner_team, ...),
    R046 = rule_r046_child_without_parent(domain = domain, key_vars = key_vars, subject_var = subject_var, group_var = group_var, impossible_combos = impossible_combos, inconsistent_pairs = inconsistent_pairs, categorical_vars = categorical_vars, seq_vars = seq_vars, placeholder_patterns = placeholder_patterns, max_distinct_values = max_distinct_values, record_count_iqr_multiplier = record_count_iqr_multiplier, owner_team = owner_team, ...),
    R047 = rule_r047_seq_gaps(domain = domain, key_vars = key_vars, subject_var = subject_var, group_var = group_var, impossible_combos = impossible_combos, inconsistent_pairs = inconsistent_pairs, categorical_vars = categorical_vars, seq_vars = seq_vars, placeholder_patterns = placeholder_patterns, max_distinct_values = max_distinct_values, record_count_iqr_multiplier = record_count_iqr_multiplier, owner_team = owner_team, ...),
    R048 = rule_r048_record_count_outlier(domain = domain, key_vars = key_vars, subject_var = subject_var, group_var = group_var, impossible_combos = impossible_combos, inconsistent_pairs = inconsistent_pairs, categorical_vars = categorical_vars, seq_vars = seq_vars, placeholder_patterns = placeholder_patterns, max_distinct_values = max_distinct_values, record_count_iqr_multiplier = record_count_iqr_multiplier, owner_team = owner_team, ...),
    R049 = rule_r049_enrollment_assessment_mismatch(domain = domain, key_vars = key_vars, subject_var = subject_var, group_var = group_var, impossible_combos = impossible_combos, inconsistent_pairs = inconsistent_pairs, categorical_vars = categorical_vars, seq_vars = seq_vars, placeholder_patterns = placeholder_patterns, max_distinct_values = max_distinct_values, record_count_iqr_multiplier = record_count_iqr_multiplier, owner_team = owner_team, ...),
    R050 = rule_r050_record_deletions(domain = domain, key_vars = key_vars, subject_var = subject_var, group_var = group_var, impossible_combos = impossible_combos, inconsistent_pairs = inconsistent_pairs, categorical_vars = categorical_vars, seq_vars = seq_vars, placeholder_patterns = placeholder_patterns, max_distinct_values = max_distinct_values, record_count_iqr_multiplier = record_count_iqr_multiplier, owner_team = owner_team, ...)
  )

  Filter(Negate(is.null), rules)
}
