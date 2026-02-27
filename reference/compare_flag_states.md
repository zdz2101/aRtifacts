# Compare flag states between two runs

Compares two flag tables (typically outputs from
[`run_rules()`](https://zdz2101.github.io/aRtifacts/reference/run_rules.md))
and classifies each unique key as `"introduced"`, `"resolved"`, or
`"persisting"`.

## Usage

``` r
compare_flag_states(
  old_flags,
  new_flags,
  by = c("entity_id", "DOMAIN", "RULE_ID")
)
```

## Arguments

- old_flags:

  Previous run flag table.

- new_flags:

  Current run flag table.

- by:

  Character vector of key columns used to identify a finding across
  runs. Defaults to `c("entity_id", "DOMAIN", "RULE_ID")`.

## Value

A deterministic `data.frame` with columns `by` plus `status`.

## Examples

``` r
old_flags <- data.frame(
  entity_id = c("01", "02"),
  DOMAIN = c("orders", "orders"),
  RULE_ID = c("TMP-900", "TMP-900"),
  stringsAsFactors = FALSE
)
new_flags <- data.frame(
  entity_id = c("02", "03"),
  DOMAIN = c("orders", "orders"),
  RULE_ID = c("TMP-900", "TMP-900"),
  stringsAsFactors = FALSE
)

compare_flag_states(old_flags, new_flags)
#>   entity_id DOMAIN RULE_ID     status
#> 1        01 orders TMP-900   resolved
#> 2        02 orders TMP-900 persisting
#> 3        03 orders TMP-900 introduced
```
