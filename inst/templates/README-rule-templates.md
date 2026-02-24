# Rule Templates (`inst/templates`)

This folder contains copy/paste starters for adding new rules and rule packs.

## Files

- `rule-pack-template.R`  
  Starter constructor that returns a list of `CoreRule` objects.
- `template-rule-pack-test.R`  
  Matching `testthat` starter for validating pack behavior.

## Suggested workflow

1. Copy `rule-pack-template.R` into `R/` and rename the constructor.
2. Replace template rule IDs (`TMP...`) with stable IDs meaningful to your domain.
3. Update required columns and rule logic.
4. Copy `template-rule-pack-test.R` into `tests/testthat/` and rename tests.
5. Run `devtools::test()`.
6. If exporting the new constructor, run `devtools::document()`.

## API assumptions

Templates in this folder assume the current Core50 engine:

- Define rules with `new_rule()` (typically topology `A1` for row-level checks).
- Execute with `run_rules(new_rule_context(current = ...), rules)`.
- Compare findings across cuts with `compare_flag_states()`.

## Rule ID guidance

- Keep IDs stable over time.
- Avoid changing semantics of an existing ID.
- Add new IDs for new logic instead of reusing old IDs.
