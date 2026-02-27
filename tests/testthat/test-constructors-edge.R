test_that("new_domain_snapshot validates required shape and keys", {
  good <- data.frame(id = 1:2, value = c(1, 2), stringsAsFactors = FALSE)
  expect_true(S7::S7_inherits(new_domain_snapshot("X", good, key_vars = "id"), DomainSnapshot))

  bad_dup <- data.frame(id = c(1, 1), value = c(1, 2), stringsAsFactors = FALSE)
  expect_error(new_domain_snapshot("X", bad_dup, key_vars = "id"), "Rows are not unique")

  expect_error(new_domain_snapshot("X", good, key_vars = "missing_col"), "Missing key variables")
  expect_error(new_domain_snapshot("X", list(a = 1), key_vars = "id"), "must be a data.frame")
})

test_that("new_artifact_snapshot validates domains", {
  dom <- new_domain_snapshot("X", data.frame(id = 1, stringsAsFactors = FALSE), key_vars = "id")
  expect_true(S7::S7_inherits(new_artifact_snapshot(domains = list(X = dom)), ArtifactSnapshot))

  expect_error(new_artifact_snapshot(domains = list()), "must contain at least one")
  expect_error(new_artifact_snapshot(domains = list(X = data.frame(id = 1))), "must be DomainSnapshot")
})

test_that("new_rule_context validates current/previous artifact types", {
  dom <- new_domain_snapshot("X", data.frame(id = 1, stringsAsFactors = FALSE), key_vars = "id")
  art <- new_artifact_snapshot(domains = list(X = dom))

  expect_true(S7::S7_inherits(new_rule_context(current = art), RuleContext))
  expect_error(new_rule_context(current = data.frame()), "current")
  expect_error(new_rule_context(current = art, previous = data.frame()), "previous")
})

test_that("new_identity_spec validates method and keys", {
  expect_true(S7::S7_inherits(new_identity_spec(keys = "entity_id", method = "exact"), IdentitySpec))
  expect_error(new_identity_spec(keys = character(), method = "exact"), "must contain at least one")
  expect_error(new_identity_spec(keys = "entity_id", method = "bad"), "must be one of")
})

test_that("new_match_spec validates join_type and window", {
  expect_true(S7::S7_inherits(new_match_spec(by = "entity_id", join_type = "inner", window_days = 0), MatchSpec))
  expect_error(new_match_spec(by = character(), join_type = "inner", window_days = 0), "must contain at least one")
  expect_error(new_match_spec(by = "entity_id", join_type = "outer", window_days = 0), "must be one of")
  expect_error(new_match_spec(by = "entity_id", join_type = "inner", window_days = -1), "non-negative")
})

test_that("new_delta_spec validates fields/tolerance/method", {
  expect_true(S7::S7_inherits(new_delta_spec(fields = "value", abs_tol = 0, rel_tol = 0, method = "exact"), DeltaSpec))
  expect_error(new_delta_spec(fields = character(), abs_tol = 0, rel_tol = 0, method = "exact"), "must contain at least one")
  expect_error(new_delta_spec(fields = "value", abs_tol = -1, rel_tol = 0, method = "exact"), "non-negative")
  expect_error(new_delta_spec(fields = "value", abs_tol = 0, rel_tol = -1, method = "exact"), "non-negative")
  expect_error(new_delta_spec(fields = "value", abs_tol = 0, rel_tol = 0, method = "bad"), "must be one of")
})

test_that("new_rule validates topology-specific constraints and fn type", {
  expect_error(
    new_rule(
      id = "T-1", title = "x", topology = "A1", scope = "record", intent = "core_integrity",
      fn = function(data, context, rule) data.frame()
    ),
    "domain"
  )

  expect_error(
    new_rule(
      id = "T-2", title = "x", topology = "A3", scope = "record", intent = "core_integrity",
      domains = "LEFT", fn = function(domains, context, rule) data.frame()
    ),
    "domains"
  )

  expect_error(
    new_rule(
      id = "T-3", title = "x", topology = "A1", scope = "record", intent = "core_integrity",
      domain = "PRIMARY", fn = 123
    ),
    "must be a function"
  )

  expect_true(S7::S7_inherits(
    new_rule(
      id = "T-4", title = "x", topology = "A1", scope = "record", intent = "core_integrity",
      domain = "PRIMARY", fn = function(data, context, rule) data.frame()
    ),
    CoreRule
  ))
})
