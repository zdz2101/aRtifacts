# GRP-002: Subject with excessive event frequency

GRP-002: Subject with excessive event frequency

## Usage

``` r
rule_grp_002_subject_excessive_event_frequency(
  domain,
  subject_var = "entity_id",
  max_events = 20,
  owner_team = "Data Operations"
)
```

## Arguments

- domain:

  Domain name.

- subject_var:

  Subject variable.

- max_events:

  Maximum allowed events per subject.

- owner_team:

  Owner team.

## Value

A `CoreRule` object.
