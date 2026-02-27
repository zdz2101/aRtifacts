# Diff key rows between two domain snapshots

Compares key combinations between an old and new `DomainSnapshot`.

## Usage

``` r
diff_keys(old_dom, new_dom)
```

## Arguments

- old_dom:

  Previous `DomainSnapshot`.

- new_dom:

  Current `DomainSnapshot`.

## Value

A list with two `data.frame` elements: `added` and `removed`.

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
diff_keys(old_dom, new_dom)
#> $added
#>   entity_id record_id
#> 1      E001         1
#> 2      E001         2
#> 3      E002         1
#> 4      E003         1
#> 5      E999       999
#> 
#> $removed
#>   entity_id record_id
#> 1      E001         1
#> 2      E001         2
#> 3      E002         1
#> 4      E003         1
#> 
```
