#' Run topology-dispatched rules
#'
#' Unified rule runner for Core50. Rules are dispatched based on `topology`
#' (`A0`-`A7`) and receive the smallest relevant payload plus full context.
#'
#' @param context A `RuleContext` or current `ArtifactSnapshot`.
#' @param rules List of `CoreRule` objects.
#'
#' @return A `data.frame` of findings with metadata columns
#'   (`RULE_ID`, `RULE_TITLE`, `TOPOLOGY`, `SCOPE`, `INTENT`, `SEVERITY`,
#'   `OWNER_TEAM`).
#' @export
run_rules <- function(context, rules) {
  rule_context_class <- get("RuleContext", mode = "function", inherits = TRUE)
  core_rule_class <- get("CoreRule", mode = "function", inherits = TRUE)

  if (S7::S7_inherits(context, ArtifactSnapshot)) {
    context <- rule_context_class(current = context, previous = NULL, options = list())
  }

  if (!S7::S7_inherits(context, rule_context_class)) {
    stop("`context` must be a RuleContext or ArtifactSnapshot.", call. = FALSE)
  }

  if (!is.list(rules) || !length(rules)) {
    return(data.frame())
  }

  out <- list()
  k <- 0L

  for (rule in rules) {
    if (!S7::S7_inherits(rule, core_rule_class)) {
      stop("All entries in `rules` must be CoreRule objects.", call. = FALSE)
    }

    result <- .dispatch_topology(rule, context)

    if (is.null(result) || !is.data.frame(result) || !nrow(result)) {
      next
    }

    result$RULE_ID <- rule@id
    result$RULE_TITLE <- rule@title
    result$TOPOLOGY <- rule@topology
    result$SCOPE <- rule@scope
    result$INTENT <- rule@intent
    result$SEVERITY <- rule@severity
    result$OWNER_TEAM <- rule@owner_team

    k <- k + 1L
    out[[k]] <- result
  }

  if (!k) {
    return(data.frame())
  }

  do.call(rbind, out)
}

#' Register a core rule
#'
#' Registers a `CoreRule` for lookup and pack composition.
#'
#' @param rule A `CoreRule`.
#' @param overwrite Whether to overwrite an existing rule with the same ID.
#'
#' @return Invisibly returns the registered rule ID.
#' @export
register_rule <- function(rule, overwrite = FALSE) {
  core_rule_class <- get("CoreRule", mode = "function", inherits = TRUE)

  if (!S7::S7_inherits(rule, core_rule_class)) {
    stop("`rule` must be a CoreRule object.", call. = FALSE)
  }

  id <- rule@id
  if (exists(id, envir = .core_rule_registry, inherits = FALSE) && !isTRUE(overwrite)) {
    stop("Rule `", id, "` is already registered. Set `overwrite = TRUE` to replace it.", call. = FALSE)
  }

  assign(id, rule, envir = .core_rule_registry)
  invisible(id)
}

#' Get a registered core rule
#'
#' @param id Rule ID.
#'
#' @return A `CoreRule` object.
#' @export
get_rule <- function(id) {
  if (!exists(id, envir = .core_rule_registry, inherits = FALSE)) {
    stop("Rule `", id, "` is not registered.", call. = FALSE)
  }

  get(id, envir = .core_rule_registry, inherits = FALSE)
}

#' List registered core rules
#'
#' @return Character vector of registered rule IDs.
#' @export
list_rules <- function() {
  sort(ls(envir = .core_rule_registry, all.names = FALSE))
}

#' Clear registered core rules
#'
#' @return Invisibly returns `TRUE`.
#' @export
clear_rule_registry <- function() {
  ids <- ls(envir = .core_rule_registry, all.names = TRUE)
  if (length(ids)) {
    rm(list = ids, envir = .core_rule_registry)
  }
  invisible(TRUE)
}

#' Build Core50 rule pack
#'
#' Returns all registered rules, or a selected subset by ID.
#'
#' @param ids Optional rule IDs to include.
#'
#' @return A list of `CoreRule` objects.
#' @export
pack_core_50 <- function(ids = NULL) {
  available <- list_rules()
  if (!length(available)) {
    return(list())
  }

  if (is.null(ids)) {
    ids <- available
  }

  missing <- setdiff(ids, available)
  if (length(missing)) {
    stop("Unknown rule IDs: ", paste(missing, collapse = ", "), ".", call. = FALSE)
  }

  lapply(ids, get_rule)
}

.dispatch_topology <- function(rule, context) {
  topology <- rule@topology

  if (topology %in% c("A1", "A2")) {
    dom <- .get_current_domain(context, rule@domain)
    if (is.null(dom)) {
      return(NULL)
    }
    return(.invoke_rule_fn(rule@fn, list(data = dom@data, context = context, rule = rule)))
  }

  if (topology == "A0") {
    if (nzchar(rule@domain)) {
      dom <- .get_current_domain(context, rule@domain)
      if (is.null(dom)) {
        return(NULL)
      }
      return(.invoke_rule_fn(rule@fn, list(domain = dom, context = context, rule = rule)))
    }
    return(.invoke_rule_fn(rule@fn, list(context = context, rule = rule)))
  }

  if (topology %in% c("A3", "A4")) {
    current_domains <- .get_current_domains(context, rule)
    if (!length(current_domains)) {
      return(NULL)
    }
    payload <- lapply(current_domains, function(dom) dom@data)
    return(.invoke_rule_fn(rule@fn, list(domains = payload, context = context, rule = rule)))
  }

  if (topology %in% c("A5", "A6")) {
    if (is.null(context@previous)) {
      return(NULL)
    }

    cur <- .get_current_domain(context, rule@domain)
    prev <- .get_previous_domain(context, rule@domain)
    if (is.null(cur) || is.null(prev)) {
      return(NULL)
    }

    return(.invoke_rule_fn(rule@fn, list(previous = prev@data, current = cur@data, context = context, rule = rule)))
  }

  if (topology == "A7") {
    if (is.null(context@previous)) {
      return(NULL)
    }

    current_domains <- .get_current_domains(context, rule)
    previous_domains <- .get_previous_domains(context, rule)
    if (!length(current_domains) || !length(previous_domains)) {
      return(NULL)
    }

    cur_payload <- lapply(current_domains, function(dom) dom@data)
    prev_payload <- lapply(previous_domains, function(dom) dom@data)

    return(.invoke_rule_fn(rule@fn, list(previous = prev_payload, current = cur_payload, context = context, rule = rule)))
  }

  stop("Unsupported topology: ", topology, call. = FALSE)
}

.get_current_domain <- function(context, domain) {
  if (!nzchar(domain)) {
    return(NULL)
  }

  domains <- context@current@domains
  if (!(domain %in% names(domains))) {
    return(NULL)
  }

  domains[[domain]]
}

.get_previous_domain <- function(context, domain) {
  if (is.null(context@previous) || !nzchar(domain)) {
    return(NULL)
  }

  domains <- context@previous@domains
  if (!(domain %in% names(domains))) {
    return(NULL)
  }

  domains[[domain]]
}

.get_current_domains <- function(context, rule) {
  names_needed <- unique(c(rule@domains, if (nzchar(rule@domain)) rule@domain else character()))
  if (!length(names_needed)) {
    return(list())
  }

  domains <- context@current@domains
  found <- domains[intersect(names_needed, names(domains))]
  if (length(found) != length(names_needed)) {
    return(list())
  }

  found
}

.get_previous_domains <- function(context, rule) {
  if (is.null(context@previous)) {
    return(list())
  }

  names_needed <- unique(c(rule@domains, if (nzchar(rule@domain)) rule@domain else character()))
  if (!length(names_needed)) {
    return(list())
  }

  domains <- context@previous@domains
  found <- domains[intersect(names_needed, names(domains))]
  if (length(found) != length(names_needed)) {
    return(list())
  }

  found
}

.invoke_rule_fn <- function(fn, args) {
  if (!is.function(fn)) {
    stop("Rule `fn` must be a function.", call. = FALSE)
  }

  fml <- names(formals(fn))
  if (is.null(fml)) {
    fml <- character()
  }

  if ("..." %in% fml) {
    return(do.call(fn, args))
  }

  matched <- args[intersect(names(args), fml)]
  do.call(fn, matched)
}

.core_rule_registry <- new.env(parent = emptyenv())
