# Compare record volume between two domain snapshots

Summarizes record-count changes across cuts, including absolute and
relative change.

## Usage

``` r
compare_record_volume(old_dom, new_dom)
```

## Arguments

- old_dom:

  Previous `DomainSnapshot`.

- new_dom:

  Current `DomainSnapshot`.

## Value

A one-row `data.frame` with deterministic columns: `domain`,
`old_n_records`, `new_n_records`, `absolute_change`, `relative_change`,
and `direction` (`"increase"`, `"decrease"`, `"unchanged"`).

## Examples

``` r
old_dom <- new_domain_snapshot(
  name = "orders",
  data = example_primary(),
  key_vars = c("entity_id", "record_id")
)
new_data <- example_primary()[1:4, , drop = FALSE]
new_dom <- new_domain_snapshot(
  name = "orders",
  data = new_data,
  key_vars = c("entity_id", "record_id")
)
vol <- compare_record_volume(old_dom, new_dom)
vol$relative_change < -0.20
#> [1] FALSE
```
