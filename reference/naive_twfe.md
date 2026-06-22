# Estimate a naive two-way fixed-effects (TWFE) event study

Runs a basic event-study TWFE regression of \`outcome\` on leads and
lags of the treatment, with unit and time fixed effects, using
\`fixest::feols()\`. The result is \*\*deliberately unsophisticated\*\*
– the point of \`nonabsdid\` is to contrast this naive benchmark against
heterogeneity-robust estimators (DCDH, \`fect\`, PanelMatch).

## Usage

``` r
naive_twfe(
  data,
  outcome,
  treatment,
  unit,
  time,
  lags = 12L,
  leads = 6L,
  controls = NULL,
  cluster = unit,
  conf.level = 0.95
)
```

## Arguments

- data:

  A data frame (panel) in long format.

- outcome, treatment, unit, time:

  Character scalars naming the outcome, the 0/1 (or \`FALSE\`/\`TRUE\`)
  treatment indicator, the unit id, and the time variable.

- lags:

  Non-negative integer: number of pre-treatment periods (event times
  \\-1, \dots, -\mathrm{lags}\\) to report. Event time \`-1\` is the
  omitted reference.

- leads:

  Non-negative integer: number of post-treatment periods (event times
  \\0, \dots, \mathrm{leads}\\) to report.

- controls:

  Optional character vector of additional control columns.

- cluster:

  Character vector of column names to cluster standard errors on.
  Defaults to \`unit\`.

- conf.level:

  Confidence level for the returned tibble. Default 0.95.

## Value

An \`nabs_event_study_tbl\` with \`method = "TWFE"\`. The fitted
\`fixest\` model is attached as the \`"fit"\` attribute.

## Details

Unlike a classic event study, \`naive_twfe()\` does \*\*not\*\* assume
the treatment is absorbing. It is built for binary treatments that can
switch on \*and off\* over time (e.g. a policy that is repealed, a
subsidy that lapses). It fits a distributed-lag TWFE in the treatment
\*levels\*, \$\$y\_{it} = \alpha_i + \gamma_t + \sum\_{k} \beta_k
D\_{i,t+k} + \varepsilon\_{it},\$\$ i.e. the outcome on the leads and
lags of the treatment indicator with unit and time fixed effects. The
coefficient on lag \`k\` is reported at event time \`+k\` and the
coefficient on lead \`k\` at event time \`-k\`, so the path is defined
relative to a treatment \*change\* rather than to a single absorbing
onset. Event time \`-1\` is the omitted reference. Each \\\beta_k\\ is a
partial correlation, not a heterogeneity-robust dynamic effect – that is
the point of the benchmark.

The naming of \`lags\`/\`leads\` follows the package convention used
elsewhere (and in the README): \`lags\` counts pre-periods, \`leads\`
counts post-periods, so \`lags = 6, leads = 8\` yields event times on
\`\[-6, 8\]\`.

Coefficients and standard errors are read directly from the fitted model
(clustered as requested); the reference period \`-1\` is reported as
exactly zero.

Missing treatment values are read as untreated (\`0\`) when the leads
and lags are constructed. For this naive benchmark that is usually
innocuous, but if treatment missingness is itself informative it can
bias the reference path; the heterogeneity-robust estimators handle
missingness on their own terms.

## Examples

``` r
df <- data.frame(
  id = rep(1:4, each = 8),
  yr = rep(1:8, times = 4),
  d  = c(rep(0, 8),
         0, 0, 1, 1, 1, 0, 0, 0,
         0, 0, 0, 1, 1, 1, 1, 0,
         rep(0, 8)),
  y  = rnorm(32)
)
naive_twfe(df, outcome = "y", treatment = "d",
           unit = "id", time = "yr", lags = 2, leads = 3)
#> # <nabs_event_study_tbl>: 6 rows, methods: "TWFE"
#> # A tibble: 6 × 8
#>    time estimate std.error conf.low conf.high window method outcome
#>   <int>    <dbl>     <dbl>    <dbl>     <dbl> <chr>  <chr>  <chr>  
#> 1    -2    0.828     1.08   -1.29       2.95  pre    TWFE   y      
#> 2    -1    0         0       0          0     pre    TWFE   y      
#> 3     0   -2.25      0.423  -3.08      -1.42  post   TWFE   y      
#> 4     1    0.974     0.486   0.0212     1.93  post   TWFE   y      
#> 5     2    0.824     0.972  -1.08       2.73  post   TWFE   y      
#> 6     3   -1.60      0.992  -3.55       0.341 post   TWFE   y      
```
