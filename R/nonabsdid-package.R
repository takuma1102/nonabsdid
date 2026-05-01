#' nonabsdid: Side-by-Side Event-Study Comparison for Heterogeneous DiD
#'
#' The `nonabsdid` package provides a single, consistent interface for
#' running, tidying, and plotting event-study estimates from several
#' heterogeneity-robust difference-in-differences estimators that support
#' non-absorbing (switching on/off) treatments:
#'
#' \itemize{
#'   \item **DCDH** --- de Chaisemartin & D'Haultfoeuille, via
#'     `DIDmultiplegtDYN::did_multiplegt_dyn()`.
#'   \item **PanelMatch** --- Imai, Kim & Wang, via
#'     `PanelMatch::PanelMatch()` / `PanelMatch::PanelEstimate()` with
#'     pre-treatment results from `PanelMatch::placebo_test()`.
#'   \item **IFE / Imputation** --- Liu, Wang & Xu, via `fect::fect()`.
#' }
#'
#' The user-facing API has three pieces:
#'
#' \itemize{
#'   \item [nabs_event_study()] runs one of the estimators with a unified argument
#'     set and returns its native object plus a tidy tibble.
#'   \item [as_nabs_event_study()] is an S3 generic that coerces native estimator
#'     objects into a stable tidy tibble (the *nabs_event_study_tbl* schema).
#'   \item [nabs_event_plot()] takes one or more *nabs_event_study_tbl* objects and
#'     overlays them on a single ggplot2 panel, optionally with a naive
#'     two-way fixed effects (TWFE) reference series in a neutral color.
#' }
#'
#' @section Tidy schema:
#' All tidiers return a tibble with class `c("nabs_event_study_tbl", "tbl_df", ...)`
#' and the following columns:
#' \describe{
#'   \item{`time`}{Integer relative period (0 = treatment onset).}
#'   \item{`estimate`}{Point estimate.}
#'   \item{`std.error`}{Standard error (may be `NA` when the estimator only
#'     reports CI bounds, e.g. some `fect` configurations).}
#'   \item{`conf.low`, `conf.high`}{Lower / upper bound of the
#'     `conf.level` confidence interval.}
#'   \item{`window`}{`"pre"` if `time < 0`, otherwise `"post"`.}
#'   \item{`method`}{Method label, e.g. `"DCDH"`, `"PanelMatch"`,
#'     `"IFE"`, or `"TWFE"`.}
#'   \item{`outcome`}{Outcome variable name (when known), else `NA`.}
#' }
#'
#' @keywords internal
"_PACKAGE"

# Suppress R CMD check NOTEs about NSE column names used inside ggplot / dplyr.
utils::globalVariables(c(
  "time", "estimate", "std.error", "conf.low", "conf.high",
  "window", "method", "outcome", "Time", "Estimate", "LB.CI", "UB.CI",
  "ymin", "ymax"
))
