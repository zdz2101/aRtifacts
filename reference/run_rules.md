# Run topology-dispatched rules

Unified rule runner for topology-driven rules. Rules are dispatched
based on `topology` (`A0`-`A7`) and receive the smallest relevant
payload plus full context.

## Usage

``` r
run_rules(context, rules)
```

## Arguments

- context:

  A `RuleContext` or current `ArtifactSnapshot`.

- rules:

  List of `CoreRule` objects.

## Value

A `data.frame` of findings with metadata columns (`RULE_ID`,
`RULE_TITLE`, `TOPOLOGY`, `SCOPE`, `INTENT`, `SEVERITY`, `OWNER_TEAM`).

## Details

`run_rules()` dispatches each `CoreRule` by `rule@topology` and passes
the smallest payload needed by that topology:

- `A0`: `fn(domain, context, rule)` for domain-scoped rules, or
  `fn(context, rule)` for context-only rules.

- `A1`, `A2`: `fn(data, context, rule)` where `data` is the current
  domain `data.frame`.

- `A3`, `A4`: `fn(domains, context, rule)` where `domains` is a named
  list of current-cut `data.frame`s.

- `A5`, `A6`: `fn(previous, current, context, rule)` where `previous`
  and `current` are same-domain `data.frame`s across cuts.

- `A7`: `fn(previous, current, context, rule)` where both `previous` and
  `current` are named lists of `data.frame`s across cuts.

If required domains are unavailable for a rule topology, the rule is
skipped and contributes no rows.
