# Diff column presence between two domain snapshots

Compares old vs new dataset column presence and reports columns that are
added, removed, or unchanged.

## Usage

``` r
diff_columns(old_dom, new_dom)
```

## Arguments

- old_dom:

  Previous `DomainSnapshot`.

- new_dom:

  Current `DomainSnapshot`.

## Value

A `data.frame` with `column`, `old_present`, `new_present`, `status`,
and `changed`.

## Examples

``` r
old_dom <- new_domain_snapshot(
  name = "orders",
  data = example_primary(),
  key_vars = c("entity_id", "record_id")
)
new_data <- example_primary()
new_data$NEWCOL <- "X"
new_data$delta <- NULL
new_dom <- new_domain_snapshot(
  name = "orders",
  data = new_data,
  key_vars = c("entity_id", "record_id")
)
diff_columns(old_dom, new_dom)
#>        column old_present new_present    status changed
#> 1      NEWCOL       FALSE        TRUE     added    TRUE
#> 2   entity_id        TRUE        TRUE unchanged   FALSE
#> 3  event_date        TRUE        TRUE unchanged   FALSE
#> 4    group_id        TRUE        TRUE unchanged   FALSE
#> 5 metric_code        TRUE        TRUE unchanged   FALSE
#> 6   record_id        TRUE        TRUE unchanged   FALSE
#> 7       value        TRUE        TRUE unchanged   FALSE
```
