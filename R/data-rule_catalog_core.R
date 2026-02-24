#' Core observability rule catalog dataset
#'
#' Internal dataset backing [rule_catalog_core()].
#'
#' @format A data frame with 50 rows and 4 columns:
#' \describe{
#'   \item{rule_id}{Rule identifier (`R001` to `R050`).}
#'   \item{rule_name}{Human-readable rule label.}
#'   \item{scope}{Rule scope (`Record`, `Group`, `Snapshot`).}
#'   \item{category}{High-level issue category.}
#' }
#' @keywords internal
"rule_catalog_core_data"
