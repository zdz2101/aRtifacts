# SNAP-006: Row count spike/drop

SNAP-006: Row count spike/drop

## Usage

``` r
rule_snap_006_row_count_spike_drop(
  domain = "PRIMARY",
  threshold_abs = 100,
  threshold_rel = 0.2,
  owner_team = "Data Operations"
)
```

## Arguments

- domain:

  Domain name.

- threshold_abs:

  Absolute row-count change threshold.

- threshold_rel:

  Relative row-count change threshold.

- owner_team:

  Owner team.

## Value

A `CoreRule` object.
