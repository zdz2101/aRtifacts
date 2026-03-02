# Diff artifact metadata

Compares top-level metadata fields between two `ArtifactSnapshot`
objects.

## Usage

``` r
diff_meta_artifact(old_art, new_art)
```

## Arguments

- old_art:

  Previous `ArtifactSnapshot`.

- new_art:

  Current `ArtifactSnapshot`.

## Value

A `data.frame` with `field`, `old`, `new`, and `changed`.

## Examples

``` r
dom <- new_domain_snapshot(
  name = "orders",
  data = example_primary(),
  key_vars = c("entity_id", "record_id")
)
old_art <- new_artifact_snapshot(domains = list(orders = dom), build_id = "run-001")
new_art <- new_artifact_snapshot(domains = list(orders = dom), build_id = "run-002")
diff_meta_artifact(old_art, new_art)
#>                     field        old        new changed
#> spec_version spec_version       <NA>       <NA>   FALSE
#> data_cut         data_cut 2026-03-02 2026-03-02   FALSE
#> build_id         build_id    run-001    run-002    TRUE
#> code_sha         code_sha       <NA>       <NA>   FALSE
#> inputs_hash   inputs_hash       <NA>       <NA>   FALSE
```
