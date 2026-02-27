# Build A2 intra-domain group pack

Build A2 intra-domain group pack

## Usage

``` r
pack_topology_a2(
  domain,
  key_vars,
  site_var = "group_id",
  subject_var = "entity_id",
  dup_rate_threshold = 0.1,
  missing_vars = character(),
  missingness_threshold = 0.1,
  max_events = 20,
  lab_test_var = "test_code",
  lab_value_var = "value",
  min_lab_repeats = 3,
  event_flag_var = NULL,
  outlier_k = 3,
  skew_value_var = NULL,
  skew_threshold = 2,
  owner_team = "Data Stewardship"
)
```

## Arguments

- domain:

  Domain name.

- key_vars:

  Key variables for duplicate checks.

- site_var:

  Site/group variable.

- subject_var:

  Subject variable.

- dup_rate_threshold:

  Threshold for duplicate-rate rules.

- missing_vars:

  Variables for missingness checks.

- missingness_threshold:

  Threshold for missingness checks.

- max_events:

  Maximum events per subject.

- lab_test_var:

  Lab test variable for GRP-003.

- lab_value_var:

  Lab value variable for GRP-003.

- min_lab_repeats:

  Minimum repeats for GRP-003.

- event_flag_var:

  Optional event flag for GRP-004.

- outlier_k:

  Robust z-score threshold for GRP-004.

- skew_value_var:

  Numeric variable for GRP-005.

- skew_threshold:

  Absolute skewness threshold for GRP-005.

- owner_team:

  Owner team.

## Value

List of `CoreRule` objects.

## Details

A2 rules are intra-domain grouped/aggregate checks executed as
`fn(data, context, rule)`.

This pack combines duplicate-rate (`ID-*`) and grouped
quality/plausibility (`GRP-*`) rules for a single domain.
