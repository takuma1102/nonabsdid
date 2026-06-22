# Collapse effect cells back onto an event-study path

Aggregates a \`nabs_effect_cell_tbl\` over cohorts to recover a
one-dimensional path, returning a \`nabs_event_study_tbl\` that plugs
straight into \[nabs_event_plot()\]. This makes explicit that the event
study is the cohort-collapsed view of the same cells.

## Usage

``` r
aggregate_effects(cells, by = c("event_time", "calendar_time"))
```

## Arguments

- cells:

  A \`nabs_effect_cell_tbl\`.

- by:

  Aggregation axis: \`"event_time"\` (default) or \`"calendar_time"\`.

## Value

A \`nabs_event_study_tbl\` (with \`NA\` standard errors).

## Details

Point estimates are averaged across cohorts (weighted by \`n\` when
present). Re-aggregated standard errors are \*\*not\*\* computed here –
collapsing SEs correctly needs the estimator's replicate draws – so
\`std.error\` and the CI columns are returned as \`NA\`. Use this for a
quick overlay, not for inference.

## Examples

``` r
raw <- expand.grid(cohort = 3:6, event_time = -2:4)
raw$estimate <- with(raw, ifelse(event_time < 0, 0, 0.2 * event_time))
cells <- as_nabs_effect_cells(raw, method = "FE")
aggregate_effects(cells)
#> ℹ Aggregated over cohorts; std.error is "NA" (re-aggregated SEs need replicate
#>   draws).
#> # <nabs_event_study_tbl>: 7 rows, methods: "FE"
#> # A tibble: 7 × 8
#>    time estimate std.error conf.low conf.high window method outcome
#>   <int>    <dbl>     <dbl>    <dbl>     <dbl> <chr>  <chr>  <chr>  
#> 1    -2      0          NA       NA        NA pre    FE     NA     
#> 2    -1      0          NA       NA        NA pre    FE     NA     
#> 3     0      0          NA       NA        NA post   FE     NA     
#> 4     1      0.2        NA       NA        NA post   FE     NA     
#> 5     2      0.4        NA       NA        NA post   FE     NA     
#> 6     3      0.6        NA       NA        NA post   FE     NA     
#> 7     4      0.8        NA       NA        NA post   FE     NA     
```
