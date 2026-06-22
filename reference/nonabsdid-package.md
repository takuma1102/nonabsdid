# nonabsdid: Side-by-Side Event-Study Comparison for Heterogeneous DiD

The \`nonabsdid\` package provides a single, consistent interface for
running, tidying, and plotting event-study estimates from several
heterogeneity-robust difference-in-differences estimators that support
non-absorbing (switching on/off) treatments:

## Details

- \*\*DCDH\*\* — de Chaisemartin & D'Haultfoeuille, via
  \`DIDmultiplegtDYN::did_multiplegt_dyn()\`.

- \*\*PanelMatch\*\* — Imai, Kim & Wang, via
  \`PanelMatch::PanelMatch()\` / \`PanelMatch::PanelEstimate()\` with
  pre-treatment results from \`PanelMatch::placebo_test()\`.

- \*\*IFE / Imputation\*\* — Liu, Wang & Xu, via \`fect::fect()\`.

The user-facing API has three pieces:

- \[nabs_event_study()\] runs one of the estimators with a unified
  argument set and returns its native object plus a tidy tibble.

- \[as_nabs_event_study()\] is an S3 generic that coerces native
  estimator objects into a stable tidy tibble (the
  \*nabs_event_study_tbl\* schema).

- \[nabs_event_plot()\] takes one or more \*nabs_event_study_tbl\*
  objects and overlays them on a single ggplot2 panel, optionally with a
  naive two-way fixed effects (TWFE) reference series in a neutral
  color.

## Tidy schema

All tidiers return a tibble with class \`c("nabs_event_study_tbl",
"tbl_df", ...)\` and the following columns:

- \`time\`:

  Integer relative period (0 = treatment onset).

- \`estimate\`:

  Point estimate.

- \`std.error\`:

  Standard error (may be \`NA\` when the estimator only reports CI
  bounds, e.g. some \`fect\` configurations).

- \`conf.low\`, \`conf.high\`:

  Lower / upper bound of the \`conf.level\` confidence interval.

- \`window\`:

  \`"pre"\` if \`time \< 0\`, otherwise \`"post"\`.

- \`method\`:

  Method label, e.g. \`"DCDH"\`, \`"PanelMatch"\`, \`"IFE"\`, or
  \`"TWFE"\`.

- \`outcome\`:

  Outcome variable name (when known), else \`NA\`.

## See also

Useful links:

- <https://github.com/takuma1102/nonabsdid>

- <https://takuma1102.github.io/nonabsdid/>

- Report bugs at <https://github.com/takuma1102/nonabsdid/issues>

## Author

**Maintainer**: Takuma Iwasaki <iwasakit@stanford.edu>
([ORCID](https://orcid.org/0009-0000-8782-4851))

Authors:

- Takuma Iwasaki <iwasakit@stanford.edu>
  ([ORCID](https://orcid.org/0009-0000-8782-4851))
