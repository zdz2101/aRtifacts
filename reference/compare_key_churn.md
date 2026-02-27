# Compare key churn between two domain snapshots

Summarizes key-level churn across cuts, including added and removed key
counts and rates.

## Usage

``` r
compare_key_churn(old_dom, new_dom)
```

## Arguments

- old_dom:

  Previous `DomainSnapshot`.

- new_dom:

  Current `DomainSnapshot`.

## Value

A one-row `data.frame` with deterministic columns: `domain`,
`old_n_keys`, `new_n_keys`, `added_n`, `removed_n`, `churn_n`,
`added_rate`, `removed_rate`, `churn_rate`, `net_change_n`, and
`net_change_rate`.

## Examples

``` r
old_dom <- new_domain_snapshot(
  name = "orders",
  data = example_primary(),
  key_vars = c("entity_id", "record_id")
)
new_data <- example_primary()
new_data <- rbind(new_data, transform(new_data[1, , drop = FALSE], entity_id = "E999", record_id = 999))
new_dom <- new_domain_snapshot(
  name = "orders",
  data = new_data,
  key_vars = c("entity_id", "record_id")
)
churn <- compare_key_churn(old_dom, new_dom)
churn$churn_rate > 0.10
#> [1] TRUE
```
