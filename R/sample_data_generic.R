#' Example primary dataset
#'
#' Creates a small synthetic generic dataset for demos and tests.
#'
#' @return A `data.frame`.
#' @export
example_primary <- function() {
  data.frame(
    entity_id = c("E001", "E001", "E002", "E003"),
    record_id = c(1, 2, 1, 1),
    metric_code = c("M1", "M2", "M1", "M1"),
    value = c(10, 20, 30, 40),
    group_id = c("G1", "G1", "G1", "G2"),
    event_date = c("2025-01-01", "2025-01-02", "2025-01-01", "2025-01-03"),
    stringsAsFactors = FALSE
  )
}
