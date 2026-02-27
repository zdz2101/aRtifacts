# Diff key variable types between two domain snapshots

Compares classes of key variables between an old and new
`DomainSnapshot`.

## Usage

``` r
diff_key_types(old_dom, new_dom)
```

## Arguments

- old_dom:

  Previous `DomainSnapshot`.

- new_dom:

  Current `DomainSnapshot`.

## Value

A `data.frame` with `key_var`, `old_type`, `new_type`, and `changed`.

## Examples

``` r
old_dom <- new_domain_snapshot(
  name = "orders",
  data = example_primary(),
  key_vars = c("entity_id", "record_id")
)
new_data <- example_primary()
new_data$entity_id <- as.factor(new_data$entity_id)
new_dom <- new_domain_snapshot(
  name = "orders",
  data = new_data,
  key_vars = c("entity_id", "record_id")
)
diff_key_types(old_dom, new_dom)
#>             key_var  old_type new_type changed
#> entity_id entity_id character   factor    TRUE
#> record_id record_id   numeric  numeric   FALSE
```
