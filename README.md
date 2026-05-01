# nonabsdid

<!-- badges: start -->
[![R-CMD-check](https://github.com/takuma1102/nonabsdid/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/takuma1102/nonabsdid/actions/workflows/R-CMD-check.yaml)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

`nonabsdid` is an R package for visualizing and comparing heterogeneity-robust staggered DID event-study estimates under **non-absorbing** and **binary** treatment.

It uses existing estimators and runs existing ones via their
own packages, then puts their output on the same time axis, the same
tidy schema, and the same ggplot2 panel so you can compare them at a glance.

Supported estimators:

- **DCDH** — de Chaisemartin & D'Haultfoeuille, via [`DIDmultiplegtDYN`](https://cran.r-project.org/package=DIDmultiplegtDYN).
- **PanelMatch** — Imai, Kim, & Wang, via [`PanelMatch`](https://cran.r-project.org/package=PanelMatch).
- **fect family** — Liu, Wang, & Xu, via [`fect`](https://cran.r-project.org/package=fect):
    - `IFE` (interactive fixed effects)
    - `FE`  (two-way fixed-effects imputation)
    - `MC`  (matrix completion)

Plus an optional **naive TWFE reference series** (via `fixest`) drawn in a
neutral color so you can see what the heterogeneity-robust estimators are
correcting against.

## Installation

```r
# Development version from GitHub:
# install.packages("pak")
pak::pak("takuma1102/nonabsdid")
```

The estimator packages themselves (`DIDmultiplegtDYN`, `PanelMatch`,
`fect`, `fixest`) are listed in `Suggests`, so install only the ones you
plan to use.

## The 30-second version

For a first look at your data, use `nabs_event_study_simple()`. It runs
the heterogeneity-robust estimators with reasonable defaults, fits a TWFE
reference, and gives you a single overlay plot to inspect:

```r
library(nonabsdid)

res <- nabs_event_study_simple(
  mydata,
  outcome   = "y",
  treatment = "d",
  unit      = "id",
  time      = "t"
)

res$plot       # the figure
res$tidy       # combined tidy tibble across methods
res$per_method # per-method tidy tibbles
res$fits       # the native estimator objects, for diagnostics
```

If a particular estimator's package is not installed, that estimator is
skipped with a message, and the remaining methods still produce output.
This is intentional: `_simple()` is for getting *something* to look at
even when your environment isn't fully provisioned.

## Careful runs

For publication-ready work, switch to the full wrapper or to the underlying
packages directly. The unified wrapper:

```r
res_dcdh <- nabs_event_study(mydata,
                             outcome = "y", treatment = "d",
                             unit = "id", time = "t",
                             method = "DCDH",
                             lags = 6, leads = 8,
                             controls = c("x1", "x2"))

res_pm   <- nabs_event_study(mydata, ..., method = "PanelMatch")
res_ife  <- nabs_event_study(mydata, ..., method = "IFE")
res_fe   <- nabs_event_study(mydata, ..., method = "FE")
res_mc   <- nabs_event_study(mydata, ..., method = "MC")
```

Or call estimators directly and tidy their output:

```r
fit <- DIDmultiplegtDYN::did_multiplegt_dyn(
  df = mydata, outcome = "y", group = "id", time = "t",
  treatment = "d", effects = 8, placebo = 6
)
tidy_dcdh <- as_nabs_event_study(fit, outcome = "y")

# Naive TWFE reference for the plot:
ref <- naive_twfe(mydata, outcome = "y", treatment = "d",
                  unit = "id", time = "t",
                  lags = 6, leads = 8)

# Overlay everything:
nabs_event_plot(
  res_dcdh$tidy, res_pm$tidy, res_ife$tidy,
  reference = ref,
  xlim = c(-6, 8), ylim = c(-2, 2),
  ylab = "Effect on outcome"
)
```

## Tidy schema

All tidiers return a tibble of class `nabs_event_study_tbl` with these columns:

| column      | type    | description                                                      |
|-------------|---------|------------------------------------------------------------------|
| `time`      | int     | Relative period (0 = treatment onset).                           |
| `estimate`  | num     | Point estimate.                                                  |
| `std.error` | num     | Standard error (may be `NA`).                                    |
| `conf.low`  | num     | Lower CI bound.                                                  |
| `conf.high` | num     | Upper CI bound.                                                  |
| `window`    | chr     | `"pre"` if `time < 0`, else `"post"`.                            |
| `method`    | chr     | `"DCDH"`, `"PanelMatch"`, `"IFE"`, `"FE"`, `"MC"`, `"TWFE"`, …   |
| `outcome`   | chr     | Outcome variable name.                                           |

Anything coercible to a data frame with at least `time` and `estimate`
columns also flows through `as_nabs_event_study()`. Adding a new estimator
later means writing a one-line method that pulls the right slots — the
plotting code keeps working.

## Status

This package is **experimental**. The output schema is intended to be stable,
but the upstream estimator packages occasionally rearrange their internal
structures, so please pin versions in production code.

## Citation

If you use this package, please also cite the underlying estimators:

- de Chaisemartin & D'Haultfœuille (2024) "Difference-in-Differences Estimators of Intertemporal Treatment Effects."
- Imai, Kim, & Wang (2023) "Matching Methods for Causal Inference with Time-Series Cross-Sectional Data." *AJPS*.
- Liu, Wang, & Xu (2024) "A Practical Guide to Counterfactual Estimators for Causal Inference with Time-Series Cross-Sectional Data." *AJPS*.
