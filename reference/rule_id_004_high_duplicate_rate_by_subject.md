# ID-004: High duplicate rate by subject

ID-004: High duplicate rate by subject

## Usage

``` r
rule_id_004_high_duplicate_rate_by_subject(
  domain,
  key_vars,
  subject_var = "entity_id",
  threshold = 0.1,
  owner_team = "Data Stewardship"
)
```

## Arguments

- domain:

  Domain name.

- key_vars:

  Key variables for duplicate detection.

- subject_var:

  Subject variable.

- threshold:

  Duplicate-rate threshold.

- owner_team:

  Owner team.

## Value

A `CoreRule` object.
