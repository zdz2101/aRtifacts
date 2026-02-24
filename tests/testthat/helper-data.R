# ---------------------------------------------------------------------------
# Shared test fixtures — sourced automatically by testthat before any test.
# ---------------------------------------------------------------------------

# Minimal valid data.frame for key-based tests
.df_orders <- function() {
  data.frame(
    order_id   = c(101L, 102L, 103L),
    customer   = c("Alice", "Bob", "Carol"),
    amount     = c(250, 480, 120),
    stringsAsFactors = FALSE
  )
}

# Pre-built DomainSnapshot from the orders fixture
.dom_orders <- function() {
  new_domain_snapshot(
    name     = "orders",
    data     = .df_orders(),
    key_vars = "order_id",
    build_id = "build-1"
  )
}

# Pre-built ArtifactSnapshot wrapping a single orders domain
.art_orders <- function(build_id = "build-1") {
  new_artifact_snapshot(
    domains  = list(orders = .dom_orders()),
    build_id = build_id
  )
}

# FlagRule that flags rows where amount > 400
.rule_high_amount <- function() {
  new_flag_rule(
    id         = "HIGH_AMOUNT",
    domain     = "orders",
    severity   = "warning",
    category   = "review",
    owner_team = "Finance",
    fn = function(df) {
      out <- df[!is.na(df$amount) & df$amount > 400, , drop = FALSE]
      if (!nrow(out)) return(data.frame())
      out[, c("order_id", "amount"), drop = FALSE]
    }
  )
}
