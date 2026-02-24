#' Build A1 intra-domain row pack
#'
#' Returns Core50 A1 rules as individual rule objects.
#'
#' @param domain Domain name.
#' @param key_vars Key variables for ID-001.
#' @param date_var Date variable for TMP-001.
#' @param start_var Start date variable for TMP-002/TMP-003.
#' @param end_var End date variable for TMP-002/TMP-003.
#' @param time_var Time variable for TMP-004.
#' @param condition_var Condition variable for DE-001.
#' @param condition_value Trigger value for DE-001.
#' @param required_var Required variable for DE-001.
#' @param numeric_var Numeric variable for DE-002.
#' @param numeric_min Lower bound for DE-002.
#' @param numeric_max Upper bound for DE-002.
#' @param categorical_var Categorical variable for DE-003.
#' @param allowed_levels Allowed levels for DE-003.
#' @param duration_var Duration variable for DE-004.
#' @param logic_var_a First variable for DE-005.
#' @param logic_val_a Disallowed value for first variable.
#' @param logic_var_b Second variable for DE-005.
#' @param logic_val_b Disallowed value for second variable.
#' @param entity_key_var Optional entity key variable in outputs.
#' @param owner_team Owner team.
#'
#' @return A list of `CoreRule` objects.
#' @export
pack_core_50_a1 <- function(domain,
                            key_vars,
                            date_var = NULL,
                            start_var = NULL,
                            end_var = NULL,
                            time_var = NULL,
                            condition_var = NULL,
                            condition_value = "Y",
                            required_var = NULL,
                            numeric_var = NULL,
                            numeric_min = -Inf,
                            numeric_max = Inf,
                            categorical_var = NULL,
                            allowed_levels = character(),
                            duration_var = NULL,
                            logic_var_a = "SEX",
                            logic_val_a = "M",
                            logic_var_b = "PREGNANT",
                            logic_val_b = "Y",
                            entity_key_var = "USUBJID",
                            owner_team = "DM") {
  rules <- list(
    rule_id_001_missing_key_fields(
      domain = domain,
      key_vars = key_vars,
      entity_key_var = entity_key_var,
      owner_team = owner_team
    )
  )

  if (!is.null(date_var)) {
    rules[[length(rules) + 1L]] <- rule_tmp_001_invalid_date_format(
      domain = domain,
      date_var = date_var,
      entity_key_var = entity_key_var,
      owner_team = owner_team
    )
  }

  if (!is.null(start_var) && !is.null(end_var)) {
    rules[[length(rules) + 1L]] <- rule_tmp_002_start_after_end(
      domain = domain,
      start_var = start_var,
      end_var = end_var,
      entity_key_var = entity_key_var,
      owner_team = owner_team
    )

    rules[[length(rules) + 1L]] <- rule_tmp_003_end_without_start(
      domain = domain,
      start_var = start_var,
      end_var = end_var,
      entity_key_var = entity_key_var,
      owner_team = owner_team
    )
  }

  if (!is.null(time_var) && !is.null(date_var)) {
    rules[[length(rules) + 1L]] <- rule_tmp_004_time_without_date(
      domain = domain,
      time_var = time_var,
      date_var = date_var,
      entity_key_var = entity_key_var,
      owner_team = owner_team
    )
  }

  if (!is.null(condition_var) && !is.null(required_var)) {
    rules[[length(rules) + 1L]] <- rule_de_001_required_field_missing_conditionally(
      domain = domain,
      condition_var = condition_var,
      required_var = required_var,
      condition_value = condition_value,
      entity_key_var = entity_key_var,
      owner_team = owner_team
    )
  }

  if (!is.null(numeric_var)) {
    rules[[length(rules) + 1L]] <- rule_de_002_out_of_range_numeric_values(
      domain = domain,
      value_var = numeric_var,
      lower = numeric_min,
      upper = numeric_max,
      entity_key_var = entity_key_var
    )
  }

  if (!is.null(categorical_var) && length(allowed_levels)) {
    rules[[length(rules) + 1L]] <- rule_de_003_invalid_categorical_code(
      domain = domain,
      value_var = categorical_var,
      allowed_levels = allowed_levels,
      entity_key_var = entity_key_var,
      owner_team = owner_team
    )
  }

  if (!is.null(duration_var)) {
    rules[[length(rules) + 1L]] <- rule_de_004_negative_duration(
      domain = domain,
      duration_var = duration_var,
      entity_key_var = entity_key_var
    )
  }

  rules[[length(rules) + 1L]] <- rule_de_005_impossible_logical_combination(
    domain = domain,
    var_a = logic_var_a,
    val_a = logic_val_a,
    var_b = logic_var_b,
    val_b = logic_val_b,
    entity_key_var = entity_key_var
  )

  rules
}

#' Register A1 intra-domain row rules
#'
#' @inheritParams pack_core_50_a1
#' @param overwrite Whether to overwrite existing IDs.
#'
#' @return Invisibly returns registered IDs.
#' @export
register_core_50_a1 <- function(domain,
                                key_vars,
                                date_var = NULL,
                                start_var = NULL,
                                end_var = NULL,
                                time_var = NULL,
                                condition_var = NULL,
                                condition_value = "Y",
                                required_var = NULL,
                                numeric_var = NULL,
                                numeric_min = -Inf,
                                numeric_max = Inf,
                                categorical_var = NULL,
                                allowed_levels = character(),
                                duration_var = NULL,
                                logic_var_a = "SEX",
                                logic_val_a = "M",
                                logic_var_b = "PREGNANT",
                                logic_val_b = "Y",
                                entity_key_var = "USUBJID",
                                owner_team = "DM",
                                overwrite = FALSE) {
  rules <- pack_core_50_a1(
    domain = domain,
    key_vars = key_vars,
    date_var = date_var,
    start_var = start_var,
    end_var = end_var,
    time_var = time_var,
    condition_var = condition_var,
    condition_value = condition_value,
    required_var = required_var,
    numeric_var = numeric_var,
    numeric_min = numeric_min,
    numeric_max = numeric_max,
    categorical_var = categorical_var,
    allowed_levels = allowed_levels,
    duration_var = duration_var,
    logic_var_a = logic_var_a,
    logic_val_a = logic_val_a,
    logic_var_b = logic_var_b,
    logic_val_b = logic_val_b,
    entity_key_var = entity_key_var,
    owner_team = owner_team
  )

  ids <- vapply(rules, function(rule) register_rule(rule, overwrite = overwrite), character(1))
  invisible(ids)
}
