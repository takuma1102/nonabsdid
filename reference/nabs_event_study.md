# Run an event-study estimator with a unified interface

\`nabs_event_study()\` is a thin wrapper around the three supported
estimators (DCDH, PanelMatch, IFE/fect) that takes a single, common
argument set and dispatches to the correct underlying package. It is
\*\*not\*\* intended to expose every option of every estimator; for
that, call the underlying packages directly and tidy their output with
\[as_nabs_event_study()\].

## Usage

``` r
nabs_event_study(
  data,
  outcome,
  treatment,
  unit,
  time,
  method = c("DCDH", "PanelMatch", "IFE", "FE", "MC"),
  lags = 6L,
  leads = 8L,
  controls = NULL,
  cluster = unit,
  conf.level = 0.95,
  cv = NULL,
  nboots = NULL,
  r = NULL,
  k = NULL,
  nlambda = NULL,
  vartype = NULL,
  se = NULL,
  parallel = FALSE,
  cores = NULL,
  number.iterations = NULL,
  se.method = NULL,
  run_placebo = NULL,
  num.cores = NULL,
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

  One of \`"DCDH"\`, \`"PanelMatch"\`, \`"IFE"\`.

- lags, leads:

  Integer pre- and post-period lengths.

- controls:

  Optional character vector of covariate names.

- cluster:

  Character; cluster variable. Defaults to \`unit\`.

- conf.level:

  Confidence level for the tidied output. Default 0.95.

- cv, nboots, r, parallel, cores:

  Tuning knobs for the \`fect\` family (\`IFE\`, \`FE\`, \`MC\`);
  ignored by other methods. \`cv\` toggles cross-validation (default: on
  for \`IFE\`/\`MC\`, off for \`FE\`); \`r\` caps / fixes the number of
  interactive-fixed-effect factors; \`nboots\` is the bootstrap count
  (default 200). \`parallel\` defaults to \`FALSE\` because, on large
  panels, copying the data to parallel workers tends to exhaust memory
  rather than help; set \`parallel = TRUE\` (optionally with \`cores\`)
  for big speedups on small panels. These are first-class arguments so
  that, e.g., \`cv = FALSE\` no longer collides with internal defaults.

- k, nlambda, vartype, se:

  Further \`fect\`-family speed knobs. \`k\` is the number of
  cross-validation rounds; \`fect\`'s own default is 20, which is slow
  on large panels, so the wrapper defaults it to 5 when CV is on.
  \`nlambda\` caps the MC regularisation grid (wrapper default 5 vs
  \`fect\`'s 10). \`vartype\` selects the variance estimator
  (\`"bootstrap"\`, \`"jackknife"\`, or \`"parametric"\`);
  \`"parametric"\` is available for \`IFE\` and avoids refitting the
  factor model on every resample, but is not supported for \`MC\`. \`se
  = FALSE\` skips uncertainty entirely for a fast point-estimate-only
  pass. Advanced \`fect\` knobs (\`tol\`, \`max.iteration\`, \`em\`,
  \`lambda\`) may also be passed through \`...\`.

- number.iterations, se.method, run_placebo, num.cores:

  Tuning knobs for \`PanelMatch\`; ignored by other methods.
  \`number.iterations\` is the bootstrap count (default 1000); lower it
  (e.g. 200) for tractability. \`se.method\` selects the SE type
  (\`"bootstrap"\`, \`"conditional"\`, \`"unconditional"\`); the
  analytic \`"conditional"\`/\`"unconditional"\` methods skip the
  bootstrap entirely and are by far the biggest speed-up. \`run_placebo
  = FALSE\` skips the separate placebo-test bootstrap (a second full
  bootstrap pass). \`parallel\`/\`num.cores\` are forwarded to
  \`PanelEstimate()\` to spread the bootstrap across cores.

- ...:

  Extra arguments passed straight to the underlying estimator.
  Stata-style aliases are also accepted here and translated with an
  informative message: \`df\` (for \`data\`), \`group\` (for \`unit\`),
  \`placebo\` (for \`lags\`), and \`effects\` (for \`leads\`; note
  \`leads = effects - 1\`, because nonabsdid places treatment onset at
  relative time 0). See the "nonabsdid for Stata users" vignette.

## Value

A list of class \`"nabs_event_study_result"\` with elements:

- \`tidy\`:

  An \`nabs_event_study_tbl\`.

- \`fit\`:

  The native estimator object (for diagnostics).

- \`call\`:

  The call that produced it.

## Details

What it does cover:

- Variable names (outcome, treatment, unit, time),

- Pre/post window length (\`lags\`, \`leads\`),

- Optional covariates and clustering,

- Reasonable defaults that match the three packages' typical use.

## Examples

``` r
 if (requireNamespace("DIDmultiplegtDYN", quietly = TRUE) &&
     requireNamespace("polars", quietly = TRUE)) {
  set.seed(1)
  library(polars)
  panel <- expand.grid(id = 1:60, t = 1:10)
  panel$d <- with(panel, as.integer(
    (id %% 4 == 1 & t %in% 4:7) |
    (id %% 4 == 2 & t %in% 5:8) |
    (id %% 4 == 3 & t %in% 6:9)
  ))
  panel$y <- 0.2 * panel$t + 0.5 * panel$d + rnorm(nrow(panel))

  res_dcdh <- nabs_event_study(
    panel,
    outcome = "y",
    treatment = "d",
    unit = "id",
    time = "t",
    method = "DCDH",
    lags = 2,
    leads = 2
  )
  res_dcdh$tidy
}

#> # <nabs_event_study_tbl>: 6 rows, methods: "DCDH"
#> # A tibble: 6 × 8
#>    time estimate std.error conf.low conf.high window method outcome
#>   <int>    <dbl>     <dbl>    <dbl>     <dbl> <chr>  <chr>  <chr>  
#> 1    -3  -0.0459        NA  -0.595      0.503 pre    DCDH   y      
#> 2    -2   0.294         NA  -0.217      0.805 pre    DCDH   y      
#> 3    -1   0             NA   0          0     pre    DCDH   y      
#> 4     0   0.493         NA  -0.0503     1.04  post   DCDH   y      
#> 5     1   0.166         NA  -0.383      0.715 post   DCDH   y      
#> 6     2   0.277         NA  -0.190      0.743 post   DCDH   y      
```
