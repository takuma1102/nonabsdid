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
#' @param cluster Character. Variable name(s) to cluster standard errors on.
#'   Defaults to `unit`.
#' @param conf.level Confidence level for the returned tibble. Default 0.95.
#'
#' @return An `nabs_event_study_tbl` with `method = "TWFE"`.
#'
#' @details
#' Internally this builds a `time_to_event` variable
#' (the difference between `time` and the first treated period for that unit)
#' and estimates
#' \deqn{Y_{it} = \alpha_i + \gamma_t + \sum_{k \neq -1} \beta_k 1\{R_{it} = k\} + X_{it}'\delta + \epsilon_{it}}
#' with `fixest::feols()` via the `i()` operator.
#'
#' The raw `time_to_event` variable is `NA` for never-treated units. For the
#' actual TWFE regression, never-treated observations are assigned to the
#' reference event time and event-time indicators are interacted with an
#' ever-treated indicator. This keeps never-treated observations in the
#' estimation sample as comparison observations while preventing them from
#' generating event-time dummy coefficients.
#'
#' This is intended to be a *reference* model. It is **not** robust to
#' treatment-effect heterogeneity and will be biased exactly in the cases
#' where DCDH / PanelMatch / IFE differ from it -- which is precisely
#' what makes it useful as a visual baseline.
#'
#' @examples
#' \donttest{
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

  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame.")
  }

  check_character_scalar(outcome, "outcome")
  check_character_scalar(treatment, "treatment")
  check_character_scalar(unit, "unit")
  check_character_scalar(time, "time")

  if (!is.null(controls)) {
    if (!is.character(controls) || any(is.na(controls))) {
      cli::cli_abort("{.arg controls} must be `NULL` or a character vector.")
    }
  }

  if (!is.character(cluster) || length(cluster) < 1L || any(is.na(cluster))) {
    cli::cli_abort("{.arg cluster} must be a character vector of column names.")
  }

  if (!is_nonnegative_integerish(lags)) {
    cli::cli_abort("{.arg lags} must be a non-negative integer.")
  }

  if (!is_nonnegative_integerish(leads)) {
    cli::cli_abort("{.arg leads} must be a non-negative integer.")
  }

  if (!is.numeric(conf.level) ||
      length(conf.level) != 1L ||
      is.na(conf.level) ||
      conf.level <= 0 ||
      conf.level >= 1) {
    cli::cli_abort("{.arg conf.level} must be a number strictly between 0 and 1.")
  }

  lags <- as.integer(lags)
  leads <- as.integer(leads)

  data <- as.data.frame(data)

  required_cols <- unique(c(outcome, treatment, unit, time, controls, cluster))
  missing_cols <- setdiff(required_cols, names(data))

  if (length(missing_cols)) {
    cli::cli_abort(
      "Column{?s} {.field {missing_cols}} not found in {.arg data}."
    )
  }

  trt <- data[[treatment]]

  if (!(is.numeric(trt) || is.logical(trt))) {
    cli::cli_abort(
      "{.arg treatment} must identify a numeric or logical 0/1 treatment indicator."
    )
  }

  trt_values <- unique(trt[!is.na(trt)])

  if (length(trt_values) && any(!(as.numeric(trt_values) %in% c(0, 1)))) {
    cli::cli_abort(
      "{.arg treatment} must be coded as 0/1, FALSE/TRUE, or contain missing values."
    )
  }

  # We only need fixest for the actual fit; check after input validation
  # so that user errors don't get masked by an "install fixest" message.
  rlang::check_installed("fixest", reason = "to fit the naive TWFE reference.")

  # Build raw relative-time variable.
  # Never-treated units remain NA here by design.
  data <- build_time_to_event(
    data,
    treatment = treatment,
    unit = unit,
    time = time
  )

  if (all(is.na(data$time_to_event))) {
    cli::cli_abort(
      "No treated observations found in {.field {treatment}}; cannot estimate a TWFE event study."
    )
  }

  # Keep never-treated observations in the regression sample.
  #
  # Raw time_to_event is NA for never-treated units. Passing that NA directly
  # through fixest::i() is fragile and can drop comparison observations.
  # Instead:
  #   - mark ever-treated observations with .__nabs_ever = 1;
  #   - set never-treated observations to the reference event time -1;
  #   - interact event-time dummies with .__nabs_ever.
  #
  # This leaves never-treated observations in the model but with zero
  # contribution to the event-time dummy columns.
  data$.__nabs_ever <- as.integer(!is.na(data$time_to_event))
  data$time_to_event <- ifelse(
    data$.__nabs_ever == 1L,
    data$time_to_event,
    -1L
  )

  # Construct the fixest formula.
  #
  # IMPORTANT: estimate dummies for *all* event times; do NOT use fixest's
  # `keep=` to restrict the window here. Restricting with `keep=` would leave
  # the out-of-window treated observations in the sample, where they get
  # absorbed into the reference category (-1) and bias every coefficient by a
  # roughly constant amount. Estimating all event-time dummies keeps the
  # reference period clean; the requested [-lags, leads] window is applied
  # afterwards, by trimming the tidy output.
  ctrl_part <- if (length(controls)) {
    paste(" + ", paste(backtick_name(controls), collapse = " + "))
  } else {
    ""
  }

  rhs <- sprintf(
    "i(time_to_event, .__nabs_ever, ref = -1)%s",
    ctrl_part
  )

  fml <- stats::as.formula(sprintf(
    "%s ~ %s | %s + %s",
    backtick_name(outcome),
    rhs,
    backtick_name(unit),
    backtick_name(time)
  ))

  fit <- fixest::feols(
    fml = fml,
    data = data,
    cluster = cluster
  )

  out <- as_nabs_event_study(
    fit,
    method = "TWFE",
    outcome = outcome,
    conf.level = conf.level
  )

  # Trim to the requested [-lags, leads] window. Coefficients are estimated
  # for all event times (to keep the reference period uncontaminated), so the
  # windowing happens here. Preserve the tibble's class and attributes, which
  # `[` would otherwise drop.
  cls <- class(out)
  cl  <- attr(out, "conf.level")
  out <- out[out$time >= -lags & out$time <= leads, , drop = FALSE]
  class(out) <- cls
  attr(out, "conf.level") <- cl

  attr(out, "fit") <- fit

  out
}

#' @rdname as_nabs_event_study
#'
#' @details
#' ## fixest method
#'
#' Extracts coefficients on `time_to_event` interactions built by [naive_twfe()]
#' with `fixest::i()`. Standard errors come from the model's clustered VCOV;
#' confidence intervals use the normal approximation and `conf.level`.
#'
#' If you call `as_nabs_event_study()` directly on a `fixest` object, the model
#' must contain coefficients of the form `time_to_event::<k>` or
#' `time_to_event::<k>:<interaction_variable>`. These are the coefficient-name
#' patterns produced by `fixest::i()`.
#'
#' @export
as_nabs_event_study.fixest <- function(x, method = NULL, outcome = NA_character_,
                                       conf.level = 0.95, ...) {
  rlang::check_installed("fixest")

  ct <- summary(x)$coeftable

  if (is.null(ct)) {
    cli::cli_abort("fixest fit has no coefficient table.")
  }

  rn <- rownames(ct)

  # Accept both:
  #   time_to_event::-3
  #   time_to_event::-3:.__nabs_ever
  #
  # The second form is produced by:
  #   i(time_to_event, .__nabs_ever, ref = -1, keep = ...)
  pat <- "^time_to_event::(-?[0-9]+)(:.*)?$"
  is_event <- grepl(pat, rn)

  if (!any(is_event)) {
    cli::cli_abort(c(
      "No event-study coefficients found.",
      "i" = "Expected coefficient names like {.val time_to_event::-3} or {.val time_to_event::-3:.__nabs_ever}.",
      "i" = "Got coefficient name{?s}: {.val {rn}}."
    ))
  }

  t <- as.integer(sub(pat, "\\1", rn[is_event]))

  est <- ct[is_event, "Estimate"]
  se <- ct[is_event, "Std. Error"]

  # Insert reference period t = -1 with beta = 0.
  # fixest drops the reference category by construction, so it usually will
  # not appear in the coefficient table.
  if (!any(t == -1L)) {
    t <- c(t, -1L)
    est <- c(est, 0)
    se <- c(se, 0)
  }

  ord <- order(t)

  new_event_study_tbl(
    time = t[ord],
    estimate = est[ord],
    std.error = se[ord],
    method = method %||% "TWFE",
    outcome = outcome,
    conf.level = conf.level
  )
}

# Build a per-unit "time_to_event" variable:
#   time_to_event = time - first treated period for that unit.
#
# Never-treated units get NA throughout. This function intentionally returns
# raw event time. The conversion needed for fixest estimation is handled inside
# naive_twfe(), not here, so tests can still verify the raw relative-time logic.
build_time_to_event <- function(data, treatment, unit, time) {
  d <- data

  d$.__nabs_rowid <- seq_len(nrow(d))
  d$.__nabs_trt <- d[[treatment]]
  d$.__nabs_unit <- d[[unit]]
  d$.__nabs_time <- d[[time]]

  # First treated period per unit.
  # Treat any non-zero treatment as treated, so logical TRUE also works.
  treated_rows <- d[
    !is.na(d$.__nabs_trt) &
      d$.__nabs_trt != 0 &
      !is.na(d$.__nabs_time),
    ,
    drop = FALSE
  ]

  if (nrow(treated_rows) == 0L) {
    d$time_to_event <- NA_integer_

    d$.__nabs_rowid <- NULL
    d$.__nabs_trt <- NULL
    d$.__nabs_unit <- NULL
    d$.__nabs_time <- NULL

    return(d)
  }

  first_treat <- stats::aggregate(
    .__nabs_time ~ .__nabs_unit,
    data = treated_rows,
    FUN = min
  )

  names(first_treat) <- c(".__nabs_unit", ".__nabs_first_t")

  d <- merge(
    d,
    first_treat,
    by = ".__nabs_unit",
    all.x = TRUE,
    sort = FALSE
  )

  d <- d[order(d$.__nabs_rowid), , drop = FALSE]

  d$time_to_event <- ifelse(
    is.na(d$.__nabs_first_t),
    NA_integer_,
    as.integer(d$.__nabs_time - d$.__nabs_first_t)
  )

  # Tidy up.
  d$.__nabs_rowid <- NULL
  d$.__nabs_trt <- NULL
  d$.__nabs_unit <- NULL
  d$.__nabs_time <- NULL
  d$.__nabs_first_t <- NULL

  d
}

check_character_scalar <- function(x, arg) {
  if (!is.character(x) || length(x) != 1L || is.na(x)) {
    cli::cli_abort("{.arg {arg}} must be a non-missing character scalar.")
  }

  invisible(x)
}

is_nonnegative_integerish <- function(x) {
  is.numeric(x) &&
    length(x) == 1L &&
    !is.na(x) &&
    is.finite(x) &&
    x >= 0 &&
    x == floor(x)
}

backtick_name <- function(x) {
  paste0("`", gsub("`", "``", x, fixed = TRUE), "`")
}
