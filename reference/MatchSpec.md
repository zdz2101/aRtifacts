# Match spec S7 class

Defines cross-domain matching logic within a snapshot.

## Usage

``` r
MatchSpec(
  by = character(0),
  join_type = character(0),
  window_days = integer(0)
)
```

## Arguments

- by:

  Character vector of join keys.

- join_type:

  Join type (`"inner"`, `"left"`, `"anti"`, `"semi"`).

- window_days:

  Optional time-window in days for temporal joins.

## Value

An S7 class object.
