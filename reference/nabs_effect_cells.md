# Fit an estimator and return cohort-by-time effect cells

\`nabs_effect_cells()\` is the cohort-matrix counterpart to
\[nabs_event_study()\]: it fits one supported estimator and returns the
result already tidied into the \`nabs_effect_cell_tbl\` schema, ready
for \[plot_effect_matrix()\]. It wires up the per-estimator machinery
that a cohort breakdown needs – a unit-level onset cohort for \`DCDH\`,
and \`keep.sims = TRUE\` for \`fect\` bootstrap cell SEs – so you do not
have to.

## Usage

``` r
nabs_effect_cells(
  data,
  outcome,
  treatment,
  unit,
  time,
  method = c("DCDH", "IFE", "FE", "MC"),
  lags = 6L,
  leads = 8L,
  controls = NULL,
  cluster = unit,
  conf.level = 0.95,
  axis = c("event", "calendar"),
  dcdh_strategy = c("loop", "by"),
  nboots = 200L,
  max_cohorts = 30L,
  ...
)
```

## Arguments

- data:

  A panel data frame, or a path to a Stata \`.dta\` file (which is read
  via \[nabs_read_dta()\] with default settings).

- outcome, treatment, unit, time:

  Character column names.

- method:

  One of \`"DCDH"\`, \`"IFE"\`, \`"FE"\`, \`"MC"\`.

- lags, leads:

  Integer pre- and post-period lengths.

- controls:

  Optional character vector of covariate names.

- cluster:

  Character; cluster variable. Defaults to \`unit\`.

- conf.level:

  Confidence level for the tidied output. Default 0.95.

- axis:

  Which axis \[plot_effect_matrix()\] should default to: \`"event"\`
  (relative time, default) or \`"calendar"\`. Both columns are populated
  regardless.

- dcdh_strategy:

  How to obtain cohort-specific DCDH estimates: \* \`"loop"\` (default)
  re-estimates the event study separately for each onset cohort against
  the never-treated units (\`only_never_switchers = TRUE\`). Robust – it
  reuses the stable event-study tidier – and the control group
  (never-treated) is constant and easy to interpret. \* \`"by"\` runs a
  single \`did_multiplegt_dyn(..., by = cohort)\` call and parses its
  per-level sublists. One estimation, native DCDH controls, but it
  depends on the package's nested-output layout.

- nboots:

  Bootstrap replicates for the \`fect\` family (default 200). Bootstrap
  draws are retained (\`keep.sims = TRUE\`) so cell SEs can be formed.

- max_cohorts:

  Safety cap on the number of distinct onset cohorts before
  \`nabs_effect_cells()\` refuses to run (default 30); raise it
  deliberately.

- ...:

  Extra arguments passed straight to the underlying estimator.
  Stata-style aliases are also accepted here and translated with an
  informative message: \`df\` (for \`data\`), \`group\` (for \`unit\`),
  \`placebo\` (for \`lags\`), and \`effects\` (for \`leads\`; note
  \`leads = effects - 1\`, because nonabsdid places treatment onset at
  relative time 0). See the "nonabsdid for Stata users" vignette.

## Value

A list of class \`"nabs_effect_cells_result"\` with elements \`cells\`
(an \`nabs_effect_cell_tbl\`), \`fit\` (native object, or a list of them
for the DCDH loop), and \`call\`.

## Status

Experimental, and intentionally limited to \`DCDH\` and the \`fect\`
family (\`IFE\` / \`FE\` / \`MC\`). \`PanelMatch\` is not supported
here.

## See also

\[plot_effect_matrix()\], \[as_nabs_effect_cells()\].

## Examples

``` r
if (requireNamespace("fect", quietly = TRUE)) {
  set.seed(1)
  panel <- expand.grid(id = 1:80, t = 1:12)
  onset <- c(`1` = 4, `2` = 6, `3` = 8)[as.character(panel$id %% 4)]
  panel$d <- as.integer(!is.na(onset) & panel$t >= onset)
  panel$y <- 0.2 * panel$t + 0.4 * panel$d + rnorm(nrow(panel))
  res <- nabs_effect_cells(panel, outcome = "y", treatment = "d",
                           unit = "id", time = "t", method = "FE",
                           nboots = 50)
  res$cells
}
#> # <nabs_effect_cell_tbl>: 21 cells, 3 cohorts, methods: "FE"
#> # A tibble: 21 × 12
#>    cohort event_time calendar_time estimate std.error conf.low conf.high     n
#>     <int>      <int>         <int>    <dbl>     <dbl>    <dbl>     <dbl> <int>
#>  1      4          0             4    0.461     0.211  -0.299      0.500    20
#>  2      4          1             5    0.488     0.184  -0.279      0.418    20
#>  3      4          2             6    1.09      0.211  -0.0659     0.626    20
#>  4      4          3             7    0.427     0.275  -0.281      0.711    20
#>  5      4          4             8    0.424     0.235  -0.259      0.561    20
#>  6      4          5             9    0.512     0.334  -0.0948     1.04     20
#>  7      4          6            10    0.715     0.299  -0.337      0.735    20
#>  8      4          7            11    0.311     0.307  -0.562      0.596    20
#>  9      4          8            12    0.457     0.312  -0.268      0.952    20
#> 10      6          0             6    0.406     0.236   0.119      0.907    20
#> # ℹ 11 more rows
#> # ℹ 4 more variables: window <chr>, method <chr>, outcome <chr>,
#> #   se_method <chr>
```
