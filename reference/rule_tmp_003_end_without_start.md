# TMP-003: End date without start

TMP-003: End date without start

## Usage

``` r
rule_tmp_003_end_without_start(
  domain,
  start_var,
  end_var,
  entity_key_var = "entity_id",
  owner_team = "Data Stewardship"
)
```

## Arguments

- domain:

  Domain name.

- start_var:

  Start date variable.

- end_var:

  End date variable.

- entity_key_var:

  Optional entity key variable for output.

- owner_team:

  Owner team.

## Value

A `CoreRule` object.
