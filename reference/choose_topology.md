# Choose topology from rule shape

Helper that maps a rule's decision shape to one of the topology labels
(`A0` to `A7`).

## Usage

``` r
choose_topology(is_longitudinal = FALSE, n_domains = 1L, level = "record")
```

## Arguments

- is_longitudinal:

  Logical scalar. Use `TRUE` when the rule compares `current` versus
  `previous` snapshots.

- n_domains:

  Number of domains the rule needs (`>= 1`).

- level:

  Check level. Supported values: `"schema"`, `"record"`, `"aggregate"`,
  `"relational"`. Aliases are accepted (for example, `"row"`, `"group"`,
  `"relationship"`).

## Value

Character scalar in `c("A0","A1","A2","A3","A4","A5","A6","A7")`.

## Examples

``` r
choose_topology(is_longitudinal = FALSE, n_domains = 1, level = "schema")
#> [1] "A0"
choose_topology(is_longitudinal = FALSE, n_domains = 1, level = "record")
#> [1] "A1"
choose_topology(is_longitudinal = FALSE, n_domains = 2, level = "relational")
#> [1] "A3"
choose_topology(is_longitudinal = TRUE, n_domains = 1, level = "aggregate")
#> [1] "A6"
choose_topology(is_longitudinal = TRUE, n_domains = 2, level = "record")
#> [1] "A7"
```
