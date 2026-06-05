#' Estimate a naive two-way fixed-effects (TWFE) event study
#'
#' Runs a basic event-study TWFE regression of `outcome` on leads and lags of
#' the treatment, with unit and time fixed effects, using `fixest::feols()`.
#' The result is **deliberately unsophisticated** -- the point of
#' `nonabsdid` is to contrast this naive benchmark against heterogeneity-robust
#' estimators (DCDH, `fect`, PanelMatch).
#'
#' Unlike a classic event study, `naive_twfe()` does **not** assume the
#' treatment is absorbing. It is built for binary treatments that can switch on
#' *and off* over time (e.g. a policy that is repealed, a subsidy that lapses).
#' Internally it uses the distributed-lag formulation of Schmidheiny and
#' Siegloch (2023): the design is built from treatment *changes*
#' \eqn{\Delta D_{it} = D_{it} - D_{i,t-1}}, with the most distant lead and lag
#' "binned" using the treatment *level*, and the reported event-study path is
#' the cumulative sum of the distributed-lag coefficients. This recovers the
#' usual event-study plot when treatment happens to be absorbing, but stays
#' correct when it is not.
#'
#' @param data A data frame (panel) in long format.
#' @param outcome,treatment,unit,time Character scalars naming the outcome,
#'   the 0/1 (or `FALSE`/`TRUE`) treatment indicator, the unit id, and the time
#'   variable.
#' @param lags Non-negative integer: number of pre-treatment periods (event
#'   times \eqn{-1, \dots, -\mathrm{lags}}) to report. Event time `-1` is the
#'   omitted reference.
#' @param leads Non-negative integer: number of post-treatment periods (event
#'   times \eqn{0, \dots, \mathrm{leads}}) to report.
#' @param controls Optional character vector of additional control columns.
#' @param cluster Character vector of column names to cluster standard errors
#'   on. Defaults to `unit`.
#' @param conf.level Confidence level for the returned tibble. Default 0.95.
#'
#' @return An `nabs_event_study_tbl` with `method = "TWFE"`. The fitted
#'   `fixest` model is attached as the `"fit"` attribute.
#'
#' @details
#' The naming of `lags`/`leads` follows the package convention used elsewhere
#' (and in the README): `lags` counts pre-periods, `leads` counts post-periods,
#' so `lags = 6, leads = 8` yields event times on `[-6, 8]`.
#'
#' Standard errors for the cumulative event-study coefficients are obtained from
#' the clustered variance-covariance matrix of the distributed-lag coefficients
#' by the delta method (each event-study coefficient is a fixed linear
#' combination of the distributed-lag coefficients).
#'
#' @references
#' Schmidheiny, K., & Siegloch, S. (2023). On event studies and
#' distributed-lags in two-way fixed effects models: Identification, equivalence,
#' and generalization. *Journal of Applied Econometrics*, 38(5), 695-713.
#'
#' @examplesIf rlang::is_installed("fixest")
#' df <- data.frame(
#'   id = rep(1:4, each = 8),
#'   yr = rep(1:8, times = 4),
#'   d  = c(rep(0, 8),
#'          0, 0, 1, 1, 1, 0, 0, 0,
#'          0, 0, 0, 1, 1, 1, 1, 0,
#'          rep(0, 8)),
#'   y  = rnorm(32)
#' )
#' naive_twfe(df, outcome = "y", treatment = "d",
#'            unit = "id", time = "yr", lags = 2, leads = 3)
#'
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

  if (!length(trt_values) || all(as.numeric(trt_values) == 0)) {
    cli::cli_abort(
      "No treated observations found in {.field {treatment}}; cannot estimate a TWFE event study."
    )
  }

  # We only need fixest for the actual fit; check after input validation so
  # that user errors don't get masked by an "install fixest" message.
  rlang::check_installed("fixest", reason = "to fit the naive TWFE reference.")

  # Build the binned distributed-lag design (Schmidheiny & Siegloch 2023).
  # Columns are named nabs_dl_p<k> (event time +k) and nabs_dl_m<k> (event
  # time -k). Never-treated units are kept in the sample with all-zero (or
  # within-unit-constant) design columns, so they serve as clean comparisons.
  dl <- build_dl_design(
    data,
    treatment = treatment,
    unit = unit,
    time = time,
    lags = lags,
    leads = leads
  )

  design <- dl$data
  dl_cols <- names(dl$map)

  if (!length(dl_cols)) {
    cli::cli_abort(
      "No treatment variation within the requested window; cannot estimate a TWFE event study."
    )
  }

  ctrl_part <- if (length(controls)) {
    paste0(" + ", paste(backtick_name(controls), collapse = " + "))
  } else {
    ""
  }

  rhs <- paste0(paste(dl_cols, collapse = " + "), ctrl_part)

  fml <- stats::as.formula(sprintf(
    "%s ~ %s | %s + %s",
    backtick_name(outcome),
    rhs,
    backtick_name(unit),
    backtick_name(time)
  ))

  fit <- fixest::feols(
    fml = fml,
    data = design,
    cluster = cluster
  )

  # Cumulate the distributed-lag coefficients into the event-study path, with
  # delta-method standard errors from the clustered VCOV.
  es <- cumulate_dl(
    estimate = stats::coef(fit),
    vcov = stats::vcov(fit),
    map = dl$map,
    lags = lags,
    leads = leads
  )

  out <- new_event_study_tbl(
    time = es$time,
    estimate = es$estimate,
    std.error = es$std.error,
    method = "TWFE",
    outcome = outcome,
    conf.level = conf.level
  )

  attr(out, "fit") <- fit

  out
}

#' @rdname as_nabs_event_study
#'
#' @details
#' ## fixest method
#'
#' Extracts coefficients on `time_to_event` interactions of the form
#' `time_to_event::<k>` or `time_to_event::<k>:<interaction>`, the coefficient
#' names produced by `fixest::i()`. These are treated as event-study *levels*
#' (the classic absorbing-treatment parametrisation). Standard errors come from
#' the model's clustered VCOV; confidence intervals use the normal
#' approximation and `conf.level`.
#'
#' Note that [naive_twfe()] no longer fits this absorbing parametrisation
#' itself -- it uses a distributed-lag design and performs the cumulation
#' internally -- but this method is retained so that models you fit yourself
#' with `fixest::i()` can still be tidied.
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

# Build the binned distributed-lag design from treatment changes.
#
# Implements the Schmidheiny & Siegloch (2023) distributed-lag parametrisation
# for a (possibly non-absorbing) binary treatment:
#
#   * delta D_{it} = D_{it} - D_{i,t-1} is the within-unit first difference of
#     treatment (the first observed period of each unit has delta D = 0).
#   * Interior post-period columns (event times 0 .. leads-1) hold delta D at
#     the corresponding lag: nabs_dl_p<k> = delta D_{i,t-k}.
#   * The most distant post column (event time = leads) is "binned": it holds
#     the treatment LEVEL D_{i,t-leads}, absorbing all longer-run effects.
#   * Interior pre-period columns (event times -2 .. -(lags-1)) hold delta D at
#     the corresponding lead: nabs_dl_m<k> = delta D_{i,t+k}.
#   * The most distant pre column (event time = -lags) is binned as
#     (D_{i,t+lags} - 1).
#   * Event time -1 (lead 1) is the omitted reference period.
#
# Cumulating these coefficients (see cumulate_dl()) yields the event-study
# level path. Columns that are identically zero across the whole sample (a
# relative time no switch ever reaches) are dropped to avoid singularities.
#
# Returns list(data = <augmented data>, map = <named int vec col -> event time>).
build_dl_design <- function(data, treatment, unit, time, lags, leads) {
  d <- data
  n <- nrow(d)

  u <- d[[unit]]
  tm <- d[[time]]

  # 0/1 treatment as numeric; NA treatment is treated as untreated (0) so that
  # never-changing rows contribute no spurious variation and stay in-sample.
  trt <- as.numeric(d[[treatment]] != 0)
  trt[is.na(trt)] <- 0

  # First difference within unit, ordered by time.
  ord <- order(u, tm)
  u_o <- u[ord]
  trt_o <- trt[ord]

  same_unit <- c(FALSE, u_o[-1L] == u_o[-length(u_o)])
  prev <- c(NA_real_, trt_o[-length(trt_o)])
  prev[!same_unit] <- NA_real_

  dD_o <- trt_o - prev
  dD_o[is.na(dD_o)] <- 0

  dD <- numeric(n)
  dD[ord] <- dD_o

  # (unit, time)-keyed look-ups for shifted delta D and treatment level.
  key <- paste(u, tm, sep = "\r")
  dD_by_key <- stats::setNames(dD, key)
  D_by_key <- stats::setNames(trt, key)

  fetch_dD <- function(offset) {
    val <- unname(dD_by_key[paste(u, tm - offset, sep = "\r")])
    val[is.na(val)] <- 0
    as.numeric(val)
  }
  fetch_D <- function(offset) {
    val <- unname(D_by_key[paste(u, tm - offset, sep = "\r")])
    val[is.na(val)] <- 0
    as.numeric(val)
  }

  map <- integer(0)

  add_col <- function(cn, ev, col) {
    if (any(col != 0)) {
      d[[cn]] <<- col
      map[cn] <<- ev
    }
  }

  # ---- Post periods (event times 0 .. leads) ----
  if (leads >= 1L) {
    for (k in 0:(leads - 1L)) {
      add_col(sprintf("nabs_dl_p%d", k), k, fetch_dD(k))      # interior: delta D
    }
    add_col(sprintf("nabs_dl_p%d", leads), leads, fetch_D(leads)) # far bin: level
  } else {
    # leads == 0: the single contemporaneous term is the treatment level.
    add_col("nabs_dl_p0", 0L, fetch_D(0L))
  }

  # ---- Pre periods (event times -2 .. -lags); -1 is the reference ----
  if (lags >= 2L) {
    if (lags >= 3L) {
      for (k in 2:(lags - 1L)) {
        add_col(sprintf("nabs_dl_m%d", k), -k, fetch_dD(-k))  # interior: delta D
      }
    }
    # far bin: (level at t + lags) - 1
    add_col(sprintf("nabs_dl_m%d", lags), -lags, fetch_D(-lags) - 1)
  }

  list(data = d, map = map)
}

# Cumulate distributed-lag coefficients into the event-study level path.
#
# Given the fitted coefficient vector and (clustered) VCOV, the column->event
# map from build_dl_design(), and the window (lags, leads):
#
#   * post:  beta_h  =  sum_{k=0}^{h}    gamma(p k),   h = 0 .. leads
#   * ref:   beta_-1 =  0
#   * pre:   beta_-h = -sum_{k=2}^{h}    gamma(m k),   h = 2 .. lags
#
# Each beta is a fixed linear combination L of the gammas, so its standard
# error is sqrt(diag(L V L')) (the delta method). The reference period gets
# estimate 0 and standard error 0.
#
# Returns data.frame(time, estimate, std.error), ordered by time.
cumulate_dl <- function(estimate, vcov, map, lags, leads) {
  # Restrict to the distributed-lag coefficients we actually estimated.
  keep <- intersect(names(map), names(estimate))
  keep <- keep[!is.na(estimate[keep])]

  gamma <- estimate[keep]
  V <- vcov[keep, keep, drop = FALSE]

  # Helper: name of the column at a given (sign, k), or NA if not estimated.
  col_for <- function(prefix, k) {
    cn <- sprintf("nabs_dl_%s%d", prefix, k)
    if (cn %in% keep) cn else NA_character_
  }

  times <- integer(0)
  rows <- list()

  add_combo <- function(ev, cols, signs) {
    L <- stats::setNames(numeric(length(keep)), keep)
    for (j in seq_along(cols)) {
      if (!is.na(cols[j])) L[cols[j]] <- L[cols[j]] + signs[j]
    }
    times[[length(times) + 1L]] <<- ev
    rows[[length(rows) + 1L]] <<- L
  }

  # Post periods 0 .. leads: cumulative sum of p0 .. ph.
  for (h in 0:leads) {
    cols <- vapply(0:h, function(k) col_for("p", k), character(1))
    add_combo(h, cols, rep(1, length(cols)))
  }

  # Pre periods -2 .. -lags: negative cumulative sum of m2 .. m|h|.
  if (lags >= 2L) {
    for (h in 2:lags) {
      cols <- vapply(2:h, function(k) col_for("m", k), character(1))
      add_combo(-h, cols, rep(-1, length(cols)))
    }
  }

  Lmat <- do.call(rbind, rows)

  if (length(keep)) {
    est <- as.numeric(Lmat %*% gamma)
    Vc <- Lmat %*% V %*% t(Lmat)
    se <- sqrt(pmax(diag(Vc), 0))
  } else {
    est <- rep(0, length(times))
    se <- rep(NA_real_, length(times))
  }

  times <- unlist(times)

  # Reference period -1: estimate 0, SE 0.
  times <- c(times, -1L)
  est <- c(est, 0)
  se <- c(se, 0)

  ord <- order(times)
  data.frame(
    time = times[ord],
    estimate = est[ord],
    std.error = se[ord],
    stringsAsFactors = FALSE
  )
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
