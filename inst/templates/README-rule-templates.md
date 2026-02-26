# Topology Rule Templates (`inst/templates`)

This folder contains copy/paste starters for adding **domain-neutral** rule packs to `aRtifacts` core.

## Scope and flavor

- Core templates in `aRtifacts` are intentionally trial-agnostic.
- Examples use generic fields (for example, `entity_id`, `measure_value`) rather than SDTM/ADaM-specific variables.
- Trial-specific logic belongs in `aRtifacts.clinical`.

## Files

- `rule-pack-template.R`
  Starter constructor that returns a list of `CoreRule` objects.
- `template-rule-pack-test.R`
  Matching `testthat` starter for validating pack behavior.
- `topology-rule-template-a0.R` ... `topology-rule-template-a7.R`
  Minimal one-rule skeletons for each topology payload contract.

## Suggested workflow

1. Copy `rule-pack-template.R` into `R/` and rename the constructor.
2. Replace template rule IDs (`TMP...`) with stable IDs meaningful to your domain.
3. Update required columns, topology, and rule logic.
4. Copy `template-rule-pack-test.R` into `tests/testthat/` and rename tests.
5. Run `devtools::test()`.
6. If exporting the new constructor, run `devtools::document()`.

If you are deciding topology first, start with `choose_topology()` and then
copy the matching `topology-rule-template-a*.R` file.

## API assumptions

Templates in this folder assume the current topology engine:

- Define rules with `new_rule()`.
- Execute with `run_rules(new_rule_context(current = ...), rules)`.
- Compare findings across cuts with `compare_flag_states()`.
- Register packs by topology with `register_topology_a0()` through `register_topology_a7()` when needed.

## Rule ID guidance

- Keep IDs stable over time.
- Avoid changing semantics of an existing ID.
- Add new IDs for new logic instead of reusing old IDs.
