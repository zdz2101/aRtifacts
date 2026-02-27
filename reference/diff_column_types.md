# Diff shared column types between two domain snapshots

Compares classes for columns that exist in both snapshots.

## Usage

``` r
diff_column_types(old_dom, new_dom)
```

## Arguments

- old_dom:

  Previous `DomainSnapshot`.

- new_dom:

  Current `DomainSnapshot`.

## Value

A `data.frame` with `column`, `old_type`, `new_type`, and `changed`.

## Examples

``` r
old_dom <- new_domain_snapshot(
  name = "orders",
  data = example_primary(),
  key_vars = c("entity_id", "record_id")
)
new_data <- example_primary()
new_data$value <- as.character(new_data$value)
new_dom <- new_domain_snapshot(
  name = "orders",
  data = new_data,
  key_vars = c("entity_id", "record_id")
)
diff_column_types(old_dom, new_dom)
#>                  column  old_type  new_type changed
#> entity_id     entity_id character character   FALSE
#> event_date   event_date character character   FALSE
#> group_id       group_id character character   FALSE
#> metric_code metric_code character character   FALSE
#> record_id     record_id   numeric   numeric   FALSE
#> value             value   numeric character    TRUE
```
