# Compare distribution shift between two domain snapshots

Summarizes group-level distribution drift for a single numeric variable
between old and new cuts, using dependency-free metrics and optional
Kolmogorov-Smirnov statistics from `stats`.

## Usage

``` r
compare_distribution_shift(old_dom, new_dom, var, by = "group_id")
```

## Arguments

- old_dom:

  Previous `DomainSnapshot`.

- new_dom:

  Current `DomainSnapshot`.

- var:

  Single variable name to compare.

- by:

  Grouping variable name. Default `"group_id"`.

## Value

A tidy `data.frame` with one row per group and deterministic columns:
`domain`, `var`, `by`, `group`, `n_old`, `n_new`, `mean_old`,
`mean_new`, `mean_delta`, `median_old`, `median_new`, `median_delta`,
`q10_old`, `q10_new`, `q10_delta`, `q25_old`, `q25_new`, `q25_delta`,
`q75_old`, `q75_new`, `q75_delta`, `q90_old`, `q90_new`, `q90_delta`,
`ks_stat`, and `ks_p_value`.

## Examples

``` r
old_df <- data.frame(
  ID = 1:6,
  group_id = c("A", "A", "A", "B", "B", "B"),
  value = c(10, 11, 12, 20, 21, 22),
  stringsAsFactors = FALSE
)
new_df <- old_df
new_df$value[new_df$group_id == "A"] <- new_df$value[new_df$group_id == "A"] + 5

old_dom <- new_domain_snapshot("orders", old_df, key_vars = "ID")
new_dom <- new_domain_snapshot("orders", new_df, key_vars = "ID")

compare_distribution_shift(old_dom, new_dom, var = "value", by = "group_id")
#>   domain   var       by group n_old n_new mean_old mean_new mean_delta
#> 1 orders value group_id     A     3     3       11       16          5
#> 2 orders value group_id     B     3     3       21       21          0
#>   median_old median_new median_delta q10_old q10_new q10_delta q25_old q25_new
#> 1         11         16            5    10.2    15.2         5    10.5    15.5
#> 2         21         21            0    20.2    20.2         0    20.5    20.5
#>   q25_delta q75_old q75_new q75_delta q90_old q90_new q90_delta ks_stat
#> 1         5    11.5    16.5         5    11.8    16.8         5       1
#> 2         0    21.5    21.5         0    21.8    21.8         0       0
#>   ks_p_value
#> 1        0.1
#> 2        1.0
```
