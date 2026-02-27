# REL-004: Timestamp outside linked window

REL-004: Timestamp outside linked window

## Usage

``` r
rule_rel_004_timestamp_outside_linked_window(
  event_domain = "EVENTS",
  window_domain = "WINDOW",
  link_keys = c("entity_id"),
  event_time_var = "event_time",
  window_start_var = "window_start",
  window_end_var = "window_end",
  window_days = 0,
  owner_team = "Data Operations"
)
```

## Arguments

- event_domain:

  Domain containing event timestamps.

- window_domain:

  Domain containing start/end window columns.

- link_keys:

  Keys used to align event records to window records.

- event_time_var:

  Timestamp/date variable in `event_domain`.

- window_start_var:

  Window start variable in `window_domain`.

- window_end_var:

  Window end variable in `window_domain`.

- window_days:

  Allowed extension (days) on each side of the linked window.

- owner_team:

  Owner team.

## Value

A `CoreRule` object.
