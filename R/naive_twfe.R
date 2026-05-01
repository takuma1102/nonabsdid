#' Estimate a naive two-way fixed-effects (TWFE) event study
#'
#' Runs a basic event-study TWFE regression of `outcome` on leads and lags
#' of `treatment`, with unit and time fixed effects, using
#' `fixest::feols()`. The result is **deliberately unsophisticated** --
#' the point of `naive_twfe()` is to provide a reference series that can be
#' overlaid in [nabs_event_plot()] (drawn in a neutral color by default) so the
#' reader can see exactly what the heterogeneity-robust estimators are
#' correcting against.
#'
#' @param data A data frame.
#' @param outcome Character. Outcome variable name.
#' @param treatment Character. Treatment indicator variable name (0/1).
#' @param unit,time Character. Unit and time variable names.
#' @param leads,lags Integers giving the number of pre- and post-periods to
#'   estimate. Defaults `lags = 12`, `leads = 6` (the common ANES-style
#'   window). The reference period (-1) is dropped automatically.
#' @param controls Optional character vector of additional covariates to
#'   include linearly (no fixed effects).
#' @param cluster Character. Variable to cluster standard errors on.
#'   Defaults to `unit`.
#' @param conf.level Confidence level for the returned tibble. Default 0.95.
#'
#' @return An `nabs_event_study_tbl` with `method = "TWFE"`.
#'
#' @details
#' Internally this builds and `time_to_event` variable
#' (the difference between `time` and the first treated period for that unit)
#' and estimates
#' \deqn{Y_{it} = \alpha_i + \gamma_t + \sum_{k \neq -1} \beta_k 1\{R_{it} = k\} + X_{it}'\delta + \epsilon_{it}}
#' with `fixest::feols()` via the `i()` operator. For never-treated units the
#' relative time is set to `NA` so they enter only as part of the comparison
#' group.
#'
#' This is intended to be a *reference* model. It is **not** robust to
#' treatment-effect heterogeneity and will be biased exactly in the cases
#' where DCDH / PanelMatch / IFE differ from it -- which is precisely
#' what makes it useful as a visual baseline.
#'
#' @examples
#' \dontrun{
#'   ref <- naive_twfe(mydata, outcome = "y", treatment = "d",
#'                     unit = "id", time = "t",
#'                     lags = 12, leads = 6)
#'   nabs_event_plot(dcdh_tidy, panelmatch_tidy, ife_tidy, reference = ref)
#' }
#' @export
naive_twfe <- function(data, outcome, treatment, unit, time,
                       lags = 12L, leads = 6L,
                       controls = NULL,
                       cluster = unit,
                       conf.level = 0.95) {

  stopifnot(
    is.data.frame(data),
    is.character(outcome),  length(outcome)   == 1L,
    is.character(treatment),length(treatment) == 1L,
    is.character(unit),     length(unit)      == 1L,
    is.character(time),     length(time)      == 1L,
    lags  >= 0, leads >= 0
  )

  data <- as.data.frame(data)
  for (col in c(outcome, treatment, unit, time)) {
    if (!col %in% names(data)) {
      cli::cli_abort("Column {.field {col}} not found in {.arg data}.")
    }
  }

  # We only need fixest for the actual fit; check after input validation
  # so that user errors don't get masked by an "install fixest" message.
  rlang::check_installed("fixest", reason = "to fit the naive TWFE reference.")

  # Build relative-time variable.
  data <- build_time_to_event(data, treatment = treatment,
                              unit = unit, time = time)

  # Construct the fixest formula. We use i(time_to_event, ref = -1, keep = ...)
  # so that the reference period is t = -1 and only the requested window is
  # estimated.
  keep <- paste0(-lags, ":", leads)
  ctrl_part <- if (length(controls)) {
    paste(" + ", paste(controls, collapse = " + "))
  } else ""

  rhs <- sprintf("i(time_to_event, ref = -1, keep = %s)%s", keep, ctrl_part)
  fml <- stats::as.formula(
    sprintf("%s ~ %s | %s + %s", outcome, rhs, unit, time)
  )

  fit <- fixest::feols(fml, data = data, cluster = stats::reformulate(cluster))

  out <- as_nabs_event_study(fit, method = "TWFE", outcome = outcome,
                        conf.level = conf.level)
  attr(out, "fit") <- fit
  out
}

#' @rdname as_nabs_event_study
#'
#' @details
#' ## fixest method
#'
#' Extracts coefficients on `time_to_event` interactions (built by [naive_twfe()]
#' with `fixest::i()`). Standard errors come from the model's clustered VCOV;
#' confidence intervals use the normal approximation and `conf.level`.
#'
#' If you call `as_nabs_event_study()` directly on a `fixest` object, the model
#' must contain coefficients of the form `time_to_event::<k>` -- this is what
#' `fixest::i()` produces.
#'
#' @export
as_nabs_event_study.fixest <- function(x, method = NULL, outcome = NA_character_,
                                  conf.level = 0.95, ...) {
  rlang::check_installed("fixest")

  ct <- summary(x)$coeftable
  if (is.null(ct)) cli::cli_abort("fixest fit has no coefficient table.")
  rn <- rownames(ct)

  # Pattern: "time_to_event::-3", "time_to_event::5", with possible factor noise.
  pat <- "^time_to_event::(-?[0-9]+)$"
  is_event <- grepl(pat, rn)
  if (!any(is_event)) {
    cli::cli_abort(c(
      "No event-study coefficients found.",
      "i" = "Expected names like {.val time_to_event::-3}; got {.val {rn}}."
    ))
  }

  t  <- as.integer(sub(pat, "\\1", rn[is_event]))
  est <- ct[is_event, "Estimate"]
  se  <- ct[is_event, "Std. Error"]

  # Insert reference (t = -1, beta = 0).
  if (!any(t == -1L)) {
    t   <- c(t,  -1L)
    est <- c(est, 0)
    se  <- c(se,  0)
  }

  ord <- order(t)
  new_event_study_tbl(
    time      = t[ord],
    estimate  = est[ord],
    std.error = se[ord],
    method    = method %||% "TWFE",
    outcome   = outcome,
    conf.level = conf.level
  )
}

# Build a per-unit "time_to_event" variable: t - (first treated period for unit).
# Never-treated units get NA, which fixest::i() treats as not-in-keep.
build_time_to_event <- function(data, treatment, unit, time) {
  d <- data
  d$.trt  <- d[[treatment]]
  d$.unit <- d[[unit]]
  d$.time <- d[[time]]

  # First treated period per unit.
  treated_rows <- d[d$.trt == 1L & !is.na(d$.trt), ]
  first_treat  <- stats::aggregate(.time ~ .unit, data = treated_rows, FUN = min)
  names(first_treat) <- c(".unit", ".first_t")

  d <- merge(d, first_treat, by = ".unit", all.x = TRUE, sort = FALSE)
  d$time_to_event <- ifelse(is.na(d$.first_t),
                            NA_integer_,
                            as.integer(d$.time - d$.first_t))
  # Tidy up.
  d$.trt <- NULL; d$.unit <- NULL; d$.time <- NULL; d$.first_t <- NULL
  d
}
