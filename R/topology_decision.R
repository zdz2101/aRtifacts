#' Choose topology from rule shape
#'
#' Helper that maps a rule's decision shape to one of the topology labels
#' (`A0` to `A7`).
#'
#' @param is_longitudinal Logical scalar. Use `TRUE` when the rule compares
#'   `current` versus `previous` snapshots.
#' @param n_domains Number of domains the rule needs (`>= 1`).
#' @param level Check level. Supported values:
#'   `"schema"`, `"record"`, `"aggregate"`, `"relational"`.
#'   Aliases are accepted (for example, `"row"`, `"group"`, `"relationship"`).
#'
#' @return Character scalar in `c("A0","A1","A2","A3","A4","A5","A6","A7")`.
#' @export
#'
#' @examples
#' choose_topology(is_longitudinal = FALSE, n_domains = 1, level = "schema")
#' choose_topology(is_longitudinal = FALSE, n_domains = 1, level = "record")
#' choose_topology(is_longitudinal = FALSE, n_domains = 2, level = "relational")
#' choose_topology(is_longitudinal = TRUE, n_domains = 1, level = "aggregate")
#' choose_topology(is_longitudinal = TRUE, n_domains = 2, level = "record")
choose_topology <- function(is_longitudinal = FALSE,
                            n_domains = 1L,
                            level = "record") {
  if (!is.logical(is_longitudinal) || length(is_longitudinal) != 1L || is.na(is_longitudinal)) {
    stop("`is_longitudinal` must be a single non-missing logical scalar.", call. = FALSE)
  }

  if (!is.numeric(n_domains) || length(n_domains) != 1L || is.na(n_domains) || n_domains < 1 || n_domains %% 1 != 0) {
    stop("`n_domains` must be a single integer >= 1.", call. = FALSE)
  }
  n_domains <- as.integer(n_domains)

  level <- .normalize_topology_level(level)

  if (!is_longitudinal && n_domains == 1L) {
    if (level == "schema") return("A0")
    if (level == "record") return("A1")
    if (level == "aggregate") return("A2")
    stop(
      "For single-cut single-domain rules, `level` must be one of: schema, record, aggregate.",
      call. = FALSE
    )
  }

  if (!is_longitudinal && n_domains >= 2L) {
    if (level == "relational") return("A3")
    if (level == "aggregate") return("A4")
    stop(
      "For single-cut cross-domain rules, `level` must be `relational` or `aggregate`.",
      call. = FALSE
    )
  }

  if (is_longitudinal && n_domains == 1L) {
    if (level == "record") return("A5")
    if (level == "aggregate") return("A6")
    stop(
      "For longitudinal single-domain rules, `level` must be `record` or `aggregate`.",
      call. = FALSE
    )
  }

  if (is_longitudinal && n_domains >= 2L) {
    if (level == "schema") {
      stop(
        "Longitudinal cross-domain rules map to A7; `level = \"schema\"` is not supported for this shape.",
        call. = FALSE
      )
    }
    return("A7")
  }

  stop("Unsupported topology decision shape.", call. = FALSE)
}

.normalize_topology_level <- function(level) {
  if (!is.character(level) || length(level) != 1L || is.na(level) || !nzchar(trimws(level))) {
    stop("`level` must be a single non-empty character scalar.", call. = FALSE)
  }

  raw <- tolower(trimws(level))
  aliases <- c(
    schema = "schema",
    structural = "schema",
    structure = "schema",
    record = "record",
    row = "record",
    aggregate = "aggregate",
    group = "aggregate",
    summary = "aggregate",
    relational = "relational",
    relationship = "relational",
    join = "relational"
  )

  if (!(raw %in% names(aliases))) {
    stop(
      "`level` must be one of: schema, record, aggregate, relational (aliases: structural/row/group/relationship/join).",
      call. = FALSE
    )
  }

  aliases[[raw]]
}
