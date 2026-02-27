# Diff values between two domain snapshots

Compares one or more variables on matched keys between two
`DomainSnapshot` objects.

## Usage

``` r
diff_values(old_dom, new_dom, vars = "value")
```

## Arguments

- old_dom:

  Previous `DomainSnapshot`.

- new_dom:

  Current `DomainSnapshot`.

- vars:

  Character vector of variables to compare.

## Value

A `data.frame` with key columns, `var`, `old`, and `new` for changed
values.

## Examples

``` r
old_dom <- new_domain_snapshot(
  name = "orders",
  data = example_primary(),
  key_vars = c("entity_id", "record_id")
)
new_data <- example_primary()
new_data$value[new_data$entity_id == "E001" & new_data$record_id == 1] <- 141
new_dom <- new_domain_snapshot(
  name = "orders",
  data = new_data,
  key_vars = c("entity_id", "record_id")
)
diff_values(old_dom, new_dom, vars = "value")
#>   entity_id record_id   var old new
#> 1      E001         1 value  10 141
```
