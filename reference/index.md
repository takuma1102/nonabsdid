# Package index

## Run an estimator

Fit one of the supported heterogeneity-robust estimators (DCDH,
PanelMatch, fect’s IFE/FE/MC) through a single unified interface, or run
several at once for a quick first look.

- [`nabs_event_study()`](https://takuma1102.github.io/nonabsdid/reference/nabs_event_study.md)
  : Run an event-study estimator with a unified interface
- [`nabs_event_study_simple()`](https://takuma1102.github.io/nonabsdid/reference/nabs_event_study_simple.md)
  : One-line exploratory front door for non-absorbing event studies

## Naive reference

A deliberately unsophisticated two-way fixed-effects event study, drawn
as a neutral reference against the heterogeneity-robust estimators.

- [`naive_twfe()`](https://takuma1102.github.io/nonabsdid/reference/naive_twfe.md)
  : Estimate a naive two-way fixed-effects (TWFE) event study

## Tidy and plot

Coerce native estimator output onto a common tidy schema, then overlay
any combination of methods on a single ggplot2 panel.

- [`as_nabs_event_study()`](https://takuma1102.github.io/nonabsdid/reference/as_nabs_event_study.md)
  : Coerce an estimator result to a tidy event-study tibble
- [`nabs_event_plot()`](https://takuma1102.github.io/nonabsdid/reference/nabs_event_plot.md)
  : Plot one or more event-study tibbles on a single panel

## Stata interoperability

Read panels from, and write tidy results back to, Stata `.dta` files.

- [`nabs_read_dta()`](https://takuma1102.github.io/nonabsdid/reference/nabs_read_dta.md)
  : Read a Stata .dta file into an analysis-ready data frame
- [`nabs_write_dta()`](https://takuma1102.github.io/nonabsdid/reference/nabs_write_dta.md)
  : Write event-study results to a Stata .dta file

## Package overview

- [`nonabsdid`](https://takuma1102.github.io/nonabsdid/reference/nonabsdid-package.md)
  [`nonabsdid-package`](https://takuma1102.github.io/nonabsdid/reference/nonabsdid-package.md)
  : nonabsdid: Side-by-Side Event-Study Comparison for Heterogeneous DiD
