# Create a topology-aware rule

Create a topology-aware rule

## Usage

``` r
new_rule(
  id,
  title,
  topology,
  scope,
  intent,
  domain = "",
  domains = character(),
  severity = "warning",
  owner_team = "Biostats",
  identity_spec = NULL,
  match_spec = NULL,
  delta_spec = NULL,
  fn
)
```

## Arguments

- id:

  Rule ID.

- title:

  Rule title.

- topology:

  Topology tag (`A0`-`A7`).

- scope:

  Scope tag.

- intent:

  Intent tag.

- domain:

  Optional primary domain.

- domains:

  Optional multi-domain set.

- severity:

  Severity label.

- owner_team:

  Owner team label.

- identity_spec:

  Optional identity spec.

- match_spec:

  Optional match spec.

- delta_spec:

  Optional delta spec.

- fn:

  Evaluation function.

## Value

A validated `CoreRule` object.
