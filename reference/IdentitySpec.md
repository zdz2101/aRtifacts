# Identity spec S7 class

Defines identity alignment logic for cross-snapshot comparisons.

## Usage

``` r
IdentitySpec(
  keys = character(0),
  method = character(0),
  hash_var = character(0)
)
```

## Arguments

- keys:

  Character vector of identity keys.

- method:

  Matching strategy (`"exact"`, `"hash"`, `"fuzzy"`).

- hash_var:

  Optional hash column used when `method = "hash"`.

## Value

An S7 class object.
