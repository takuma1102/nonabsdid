# Coerce an estimator result to a tidy cohort-by-time effect-cell tibble

\`as_nabs_effect_cells()\` is an S3 generic that converts the native
output of a supported estimator into a \*cohort x time\* effect-cell
schema – the input for \[plot_effect_matrix()\] heatmaps. It is the
two-dimensional companion to \[as_nabs_event_study()\]: where the
event-study schema collapses everything onto a single relative-time
axis, this schema keeps the cohort (treatment onset period) as a second
dimension so heterogeneity \*across\* cohorts stays visible.

## Usage

``` r
as_nabs_effect_cells(
  x,
  method = NULL,
  outcome = NA_character_,
  conf.level = 0.95,
  ...
)

# S3 method for class 'data.frame'
as_nabs_effect_cells(
  x,
  method = NULL,
  outcome = NA_character_,
  conf.level = 0.95,
  ...
)

# S3 method for class 'did_multiplegt_dyn'
as_nabs_effect_cells(
  x,
  method = NULL,
  outcome = NA_character_,
  conf.level = 0.95,
  ...
)

# S3 method for class 'fect'
as_nabs_effect_cells(
  x,
  method = NULL,
  outcome = NA_character_,
  conf.level = 0.95,
  axis = c("event", "calendar"),
  weighted = TRUE,
  ...
)
```

## Arguments

- x:

  A supported estimator object, or a data frame with at least
  \`cohort\`, \`event_time\`, and \`estimate\` columns.

- method:

  Optional override for the \`method\` column.

- outcome:

  Optional outcome name recorded in the \`outcome\` column.

- conf.level:

  Confidence level used to derive \`conf.low\` / \`conf.high\` from
  \`std.error\` when explicit bounds are not supplied. Default \`0.95\`.

- ...:

  Method-specific arguments (e.g. \`axis\`, \`weighted\` for the
  \`fect\` method).

- axis:

  For the matrix axes only: \`"event"\` keeps \`event_time\` (default),
  \`"calendar"\` additionally fills \`calendar_time\`. Both columns are
  always present; this only affects which one \[plot_effect_matrix()\]
  defaults to.

- weighted:

  Logical; weight the within-cell mean of \`eff\` by \`W.agg\`. Default
  \`TRUE\`.

## Value

A tibble of class \`"nabs_effect_cell_tbl"\`, one row per \`(cohort,
event_time)\` cell, with columns documented in
\[new_effect_cell_tbl()\].

## Details

\## DCDH method

Expects a \`did_multiplegt_dyn\` object \*\*run with the \`by\`
option\*\*, where the \`by\` variable is a unit-level onset cohort (e.g.
each unit's first treated period). When \`by\` is set, the object is
reshaped into one sublist per \`by\` level, each carrying its own
event-study \`plot\$data\` (\`Time\`, \`Estimate\`, \`LB.CI\`,
\`UB.CI\`, and sometimes \`SE\`). This method walks those sublists and
stacks them into the cohort-by-time schema, shifting the axis so onset
sits at \`event_time = 0\` (the same \`-1\` shift the event-study tidier
applies).

Building the cohort \`by\` variable and running DCDH for you is exactly
what \[nabs_effect_cells()\] with \`method = "DCDH"\` does; call the
generic directly only when you already have a \`by\`-run object in hand.

SEs are the estimator's own (\`se_method = "native"\`) when
\`Time\`-level SEs are present in the plot data; otherwise CIs are
carried through and the SE column is \`NA\`.

\## fect method

Uses \`fect::imputed_outcomes()\` (fect \>= 2.4.0), the documented
long-form accessor that returns one row per treated cell with columns
\`id\`, \`time\`, \`event.time\`, \`cohort\`, \`eff\`, and \`W.agg\`.
The cell estimate for each \`(cohort, event_time)\` group is the
\`W.agg\`-weighted mean of the cell-level effects \`eff\` (set
\`weighted = FALSE\` for an unweighted mean).

Standard errors come from the bootstrap surface: when the fit was
produced with \`se = TRUE\` and \`keep.sims = TRUE\`,
\`imputed_outcomes(replicates = TRUE)\` is re-aggregated within each
replicate, and the cell SE is the standard deviation across replicates
(with percentile CIs). Without stored sims the SE / CI columns are
\`NA\` and \`se_method\` is \`"none"\`.

## Status

This is an \*\*experimental\*\* feature line, separate from the stable
event-study API. Only the \`fect\` family (\`IFE\` / \`FE\` / \`MC\`)
and \`DCDH\` (\`DIDmultiplegtDYN\`) are supported; \`PanelMatch\` is
deliberately omitted for now because a faithful cohort breakdown there
needs the matched-set bootstrap to be re-aggregated by cohort, which is
out of scope for this pass.

## Cohort and event-time conventions

\* \`cohort\` is the treatment \*\*onset calendar period\*\* (the first
period a unit is treated). For repeated on/off treatment this is the
\*first\* onset, so interpret later periods through the estimator's own
carryover handling. \* \`event_time\` is the relative period with \`0\`
at onset, matching the \`nabs_event_study_tbl\` convention. For \`fect\`
this is computed directly as \`calendar_time - cohort\`; for \`DCDH\` it
is the native event-study axis shifted so onset sits at \`0\`. \* The
\`fect\` surface only covers \*\*treated\*\* cells, so its matrix spans
\`event_time \>= 0\`. \`DCDH\` run with placebos additionally yields the
pre-period (\`event_time \< 0\`) cells.

## See also

\[plot_effect_matrix()\] to draw the heatmap, \[nabs_effect_cells()\] to
fit and tidy in one step, \[aggregate_effects()\] to collapse cells back
onto an event-study path.

## Examples

``` r
# The data.frame escape hatch needs no estimator packages.
raw <- expand.grid(cohort = 3:5, event_time = 0:3)
raw$estimate  <- with(raw, 0.1 * event_time + 0.05 * (cohort - 4))
raw$std.error <- 0.08
cells <- as_nabs_effect_cells(raw, method = "DCDH", outcome = "y")
cells
#> # <nabs_effect_cell_tbl>: 12 cells, 3 cohorts, methods: "DCDH"
#> # A tibble: 12 × 12
#>    cohort event_time calendar_time estimate std.error conf.low conf.high     n
#>     <int>      <int>         <int>    <dbl>     <dbl>    <dbl>     <dbl> <int>
#>  1      3          0             3    -0.05      0.08 -0.207       0.107    NA
#>  2      3          1             4     0.05      0.08 -0.107       0.207    NA
#>  3      3          2             5     0.15      0.08 -0.00680     0.307    NA
#>  4      3          3             6     0.25      0.08  0.0932      0.407    NA
#>  5      4          0             4     0         0.08 -0.157       0.157    NA
#>  6      4          1             5     0.1       0.08 -0.0568      0.257    NA
#>  7      4          2             6     0.2       0.08  0.0432      0.357    NA
#>  8      4          3             7     0.3       0.08  0.143       0.457    NA
#>  9      5          0             5     0.05      0.08 -0.107       0.207    NA
#> 10      5          1             6     0.15      0.08 -0.00680     0.307    NA
#> 11      5          2             7     0.25      0.08  0.0932      0.407    NA
#> 12      5          3             8     0.35      0.08  0.193       0.507    NA
#> # ℹ 4 more variables: window <chr>, method <chr>, outcome <chr>,
#> #   se_method <chr>
```
