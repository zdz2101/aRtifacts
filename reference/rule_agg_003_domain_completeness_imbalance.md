# AGG-003: Domain completeness imbalance

AGG-003: Domain completeness imbalance

## Usage

``` r
rule_agg_003_domain_completeness_imbalance(
  domains_set = c("PRIMARY", "SUPPORT", "AUX"),
  group_var = "group_id",
  threshold_max_gap = 0.2,
  owner_team = "Data Operations"
)
```

## Arguments

- domains_set:

  Domain set evaluated for group-level balance.

- group_var:

  Group variable used for comparison.

- threshold_max_gap:

  Maximum allowed rate gap across domains per group.

- owner_team:

  Owner team.

## Value

A `CoreRule` object.
