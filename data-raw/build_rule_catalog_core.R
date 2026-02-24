# Build script for the core observability rule catalog dataset.
#
# Run from project root:
# source("data-raw/build_rule_catalog_core.R")

csv_path <- file.path("data-raw", "rule_catalog_core.csv")
out_dir <- "data"
out_path <- file.path(out_dir, "rule_catalog_core_data.rda")

if (!file.exists(csv_path)) {
  stop("Missing source CSV: ", csv_path, call. = FALSE)
}

rule_catalog_core_data <- utils::read.csv(
  csv_path,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

expected_names <- c("rule_id", "rule_name", "scope", "category")
if (!identical(names(rule_catalog_core_data), expected_names)) {
  stop("Unexpected columns in catalog CSV.", call. = FALSE)
}

expected_ids <- sprintf("R%03d", seq_len(50))
if (!identical(rule_catalog_core_data$rule_id, expected_ids)) {
  stop("Catalog must contain ordered rule IDs R001..R050.", call. = FALSE)
}

if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE)
}

save(rule_catalog_core_data, file = out_path, compress = "bzip2")
message("Wrote ", out_path)
