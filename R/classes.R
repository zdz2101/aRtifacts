#' Domain snapshot S7 class
#'
#' Represents one keyed dataset ("domain") at a specific point in time, with
#' validation on key variables and key uniqueness. Schema-agnostic — works with
#' any tabular data, not limited to SDTM or ADaM.
#'
#' @param name Domain or table name (e.g. `"ADVS"`, `"orders"`).
#' @param data A `data.frame` containing domain rows.
#' @param key_vars Character vector of columns that uniquely identify rows.
#' @param spec_version Optional specification or schema version label.
#' @param data_cut Data cut identifier/date string.
#' @param build_id Build/run identifier.
#' @param code_sha Source code revision identifier.
#' @param inputs_hash Hash representing upstream inputs.
#'
#' @name DomainSnapshot
#' @return An S7 class object.
#' @export
DomainSnapshot <- S7::new_class(
  "DomainSnapshot",
  properties = list(
    name = S7::class_character,
    data = S7::class_any,
    key_vars = S7::class_character,
    spec_version = S7::class_character,
    data_cut = S7::class_character,
    build_id = S7::class_character,
    code_sha = S7::class_character,
    inputs_hash = S7::class_character
  ),
  validator = function(self) {
    issues <- character()

    if (!is.data.frame(self@data)) {
      issues <- c(issues, "`data` must be a data.frame.")
    }

    if (!length(self@key_vars)) {
      issues <- c(issues, "`key_vars` must contain at least one variable name.")
    }

    if (is.data.frame(self@data) && length(self@key_vars)) {
      missing_keys <- setdiff(self@key_vars, names(self@data))
      if (length(missing_keys)) {
        issues <- c(issues, paste0("Missing key variables: ", paste(missing_keys, collapse = ", "), "."))
      } else if (!.has_unique_keys(self@data, self@key_vars)) {
        issues <- c(issues, "Rows are not unique by `key_vars`.")
      }
    }

    if (length(issues)) issues else NULL
  }
)

#' Artifact snapshot S7 class
#'
#' Represents a study-level snapshot made of multiple validated
#' `DomainSnapshot` objects and shared metadata.
#'
#' @param domains Named list of `DomainSnapshot` objects.
#' @param spec_version Optional specification or standards version label.
#' @param data_cut Data cut identifier/date string.
#' @param build_id Build/run identifier.
#' @param code_sha Source code revision identifier.
#' @param inputs_hash Hash representing upstream inputs.
#'
#' @name ArtifactSnapshot
#' @return An S7 class object.
#' @export
ArtifactSnapshot <- S7::new_class(
  "ArtifactSnapshot",
  properties = list(
    domains = S7::class_list,
    spec_version = S7::class_character,
    data_cut = S7::class_character,
    build_id = S7::class_character,
    code_sha = S7::class_character,
    inputs_hash = S7::class_character
  ),
  validator = function(self) {
    issues <- character()

    if (!length(self@domains)) {
      issues <- c(issues, "`domains` must contain at least one DomainSnapshot.")
    }

    if (length(self@domains)) {
      bad <- names(self@domains)[!vapply(self@domains, function(x) S7::S7_inherits(x, DomainSnapshot), logical(1))]
      if (length(bad)) {
        issues <- c(issues, paste0("All domains must be DomainSnapshot objects. Offenders: ", paste(bad, collapse = ", "), "."))
      }
    }

    if (length(issues)) issues else NULL
  }
)

#' Flag rule S7 class
#'
#' Defines a rule that can be run against one domain in an
#' `ArtifactSnapshot`.
#'
#' @param id Rule identifier string.
#' @param domain Target domain name in the artifact.
#' @param severity Severity label (e.g. `"warning"`, `"critical"`).
#' @param category Rule category label.
#' @param owner_team Team responsible for follow-up.
#' @param fn Function that accepts a domain `data.frame` and returns a
#'   `data.frame` of flagged rows.
#'
#' @name FlagRule
#' @return An S7 class object.
#' @export
FlagRule <- S7::new_class(
  "FlagRule",
  properties = list(
    id = S7::class_character,
    domain = S7::class_character,
    severity = S7::class_character,
    category = S7::class_character,
    owner_team = S7::class_character,
    fn = S7::class_any
  ),
  validator = function(self) {
    issues <- character()
    if (!is.function(self@fn)) {
      issues <- c(issues, "`fn` must be a function.")
    }
    if (length(issues)) issues else NULL
  }
)

#' Create a domain snapshot
#'
#' Constructs and validates a `DomainSnapshot`.
#'
#' @param name Domain or table name (for example, `"ADVS"`, `"orders"`, `"lab_results"`).
#' @param data A `data.frame` containing domain rows.
#' @param key_vars Character vector of key columns that uniquely identify rows.
#' @param spec_version Optional specification or schema version label.
#' @param data_cut Data cut identifier/date string.
#' @param build_id Build/run identifier.
#' @param code_sha Source code revision identifier.
#' @param inputs_hash Hash representing upstream inputs.
#'
#' @return A validated `DomainSnapshot` object.
#' @export
#'
#' @examples
#' dom <- new_domain_snapshot(
#'   name = "ADVS",
#'   data = example_advs(),
#'   key_vars = c("USUBJID", "PARAMCD", "AVISIT")
#' )
new_domain_snapshot <- function(name,
                                data,
                                key_vars,
                                spec_version = NA_character_,
                                data_cut = as.character(Sys.Date()),
                                build_id = NA_character_,
                                code_sha = NA_character_,
                                inputs_hash = NA_character_) {
  DomainSnapshot(
    name = name,
    data = data,
    key_vars = key_vars,
    spec_version = spec_version,
    data_cut = data_cut,
    build_id = build_id,
    code_sha = code_sha,
    inputs_hash = inputs_hash
  )
}
  #' Create an artifact snapshot
  #'
  #' Constructs and validates an `ArtifactSnapshot` that contains one or more
  #' named `DomainSnapshot` objects.
  #'
  #' @param domains Named list of `DomainSnapshot` objects.
  #' @param spec_version Specification or standards version label.
  #' @param data_cut Data cut identifier/date string.
  #' @param build_id Build/run identifier.
  #' @param code_sha Source code revision identifier.
  #' @param inputs_hash Hash representing upstream inputs.
  #'
  #' @return A validated `ArtifactSnapshot` object.
  #' @export
  #'
  #' @examples
  #' dom <- new_domain_snapshot(
  #'   name = "ADVS",
  #'   data = example_advs(),
  #'   key_vars = c("USUBJID", "PARAMCD", "AVISIT")
  #' )
  #' art <- new_artifact_snapshot(domains = list(ADVS = dom))

new_artifact_snapshot <- function(domains,
                                  spec_version = NA_character_,
                                  data_cut = as.character(Sys.Date()),
                                  build_id = NA_character_,
                                  code_sha = NA_character_,
                                  inputs_hash = NA_character_) {
  ArtifactSnapshot(
    domains = domains,
    spec_version = spec_version,
    data_cut = data_cut,
    build_id = build_id,
    code_sha = code_sha,
    inputs_hash = inputs_hash
  )
}
  #' Create a flag rule
  #'
  #' Convenience constructor for `FlagRule` objects used by `run_flags()`.
  #'
  #' @param id Rule identifier.
  #' @param domain Target domain name in an artifact.
  #' @param severity Severity label (for example, `"warning"`, `"critical"`).
  #' @param category Rule category label.
  #' @param owner_team Team that owns follow-up.
  #' @param fn Function that accepts a domain `data.frame` and returns a
  #'   `data.frame` of flagged rows.
  #'
  #' @return A validated `FlagRule` object.
  #' @export
  #'
  #' @examples
  #' rule <- new_flag_rule(
  #'   id = "HI_SYSBP",
  #'   domain = "ADVS",
  #'   fn = function(df) {
  #'     out <- df[df$PARAMCD == "SYSBP" & df$AVAL > 180, , drop = FALSE]
  #'     if (!nrow(out)) return(data.frame())
  #'     out[, c("USUBJID", "PARAMCD", "AVAL"), drop = FALSE]
  #'   }
  #' )

new_flag_rule <- function(id,
                          domain,
                          severity = "warning",
                          category = "quality",
                          owner_team = "Biostats",
                          fn) {
  FlagRule(
    id = id,
    domain = domain,
    severity = severity,
    category = category,
    owner_team = owner_team,
    fn = fn
  )
}
