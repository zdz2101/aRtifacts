# Core rule S7 class

Topology-aware rule definition used by the unified topology dispatcher.

## Usage

``` r
CoreRule(
  id = character(0),
  title = character(0),
  topology = character(0),
  scope = character(0),
  intent = character(0),
  domain = character(0),
  domains = character(0),
  severity = character(0),
  owner_team = character(0),
  identity_spec = NULL,
  match_spec = NULL,
  delta_spec = NULL,
  fn = NULL
)
```

## Arguments

- id:

  Stable rule ID (for example, `"SCH-001"`).

- title:

  Short human-readable title.

- topology:

  Topology tag (`A0` to `A7`).

- scope:

  Scope tag (`record`, `group`, `snapshot`).

- intent:

  Non-empty intent label describing why the rule exists.

- domain:

  Optional primary domain name.

- domains:

  Optional domain set used by cross-domain rules.

- severity:

  Severity label.

- owner_team:

  Owner team label.

- identity_spec:

  Optional `IdentitySpec`.

- match_spec:

  Optional `MatchSpec`.

- delta_spec:

  Optional `DeltaSpec`.

- fn:

  Evaluation function used by
  [`run_rules()`](https://zdz2101.github.io/aRtifacts/reference/run_rules.md).

## Value

An S7 class object.
