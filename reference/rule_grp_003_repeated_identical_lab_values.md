# GRP-003: Repeated identical lab values

GRP-003: Repeated identical lab values

## Usage

``` r
rule_grp_003_repeated_identical_lab_values(
  domain,
  subject_var = "entity_id",
  test_var = "LBTESTCD",
  value_var = "LBSTRESN",
  min_repeats = 3,
  owner_team = "Data Operations"
)
```

## Arguments

- domain:

  Domain name.

- subject_var:

  Subject variable.

- test_var:

  Test code variable.

- value_var:

  Numeric lab value variable.

- min_repeats:

  Minimum rows in a subject/test stratum.

- owner_team:

  Owner team.

## Value

A `CoreRule` object.
