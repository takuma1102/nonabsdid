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
#' It fits a distributed-lag TWFE in the treatment *levels*,
#' \deqn{y_{it} = \alpha_i + \gamma_t + \sum_{k} \beta_k D_{i,t+k} + \varepsilon_{it},}
#' i.e. the outcome on the leads and lags of the treatment indicator with unit
#' and time fixed effects. The coefficient on lag `k` is reported at event
#' time `+k` and the coefficient on lead `k` at event time `-k`, so the path is
#' defined relative to a treatment *change* rather than to a single absorbing
#' onset. Event time `-1` is the omitted reference. Each \eqn{\beta_k} is a
#' partial correlation, not a heterogeneity-robust dynamic effect -- that is the
#' point of the benchmark.
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
#' Coefficients and standard errors are read directly from the fitted model
#' (clustered as requested); the reference period `-1` is reported as exactly
#' zero.
#'
#' Missing treatment values are read as untreated (`0`) when the leads and lags
#' are constructed. For this naive benchmark that is usually innocuous, but if
#' treatment missingness is itself informative it can bias the reference path;
#' the heterogeneity-robust estimators handle missingness on their own terms.
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

  # build_dl_design() adds columns by `[[<-`; on a data.table that would mutate
  # the caller's object by reference, so coerce non-base frames to a plain
  # data.frame. A base data.frame (the `_simple()` path already passes one) is
  # left as-is to avoid a redundant full copy.
  if (!identical(class(data), "data.frame")) data <- as.data.frame(data)

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

  # Build the distributed-lag design in treatment levels. Columns are named
  # nabs_dl_p<k> (event time +k) and nabs_dl_m<k> (event time -k). Never-treated
  # units are kept in the sample with all-zero columns as clean comparisons.
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

  # Read the per-period coefficients and (clustered) standard errors directly.
  es <- collect_dl(
    estimate = stats::coef(fit),
    se = sqrt(diag(stats::vcov(fit))),
    map = dl$map
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
#' Note that [naive_twfe()] does not fit this absorbing parametrisation itself
#' -- it uses a distributed-lag design in treatment levels -- but this method is
#' retained so that models you fit yourself with `fixest::i()` can still be
#' tidied.
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

# Build the distributed-lag design in treatment levels.
#
# For event time h in -lags..leads (omitting the reference -1), creates one
# column equal to the treatment indicator shifted by h:
#
#   * post period h (>= 0):  nabs_dl_p<h> = D_{i, t-h}   (lag h)
#   * pre period -h (>= 2):  nabs_dl_m<h> = D_{i, t+h}   (lead h)
#
# NA treatment is read as untreated (0). Columns that are identically zero are
# dropped. Never-treated units have all-zero columns and stay in the sample as
# clean comparisons.
#
# Returns list(data = <augmented data>, map = <named int vec col -> event time>).
build_dl_design <- function(data, treatment, unit, time, lags, leads) {
  d <- data

  u <- d[[unit]]
  tm <- d[[time]]

  # 0/1 treatment as numeric; NA treatment is treated as untreated (0).
  trt <- as.numeric(d[[treatment]] != 0)
  trt[is.na(trt)] <- 0

  # Integer-coded (unit, time) composite key, so each shifted look-up is a
  # single integer match() rather than a fresh full-length paste() + string
  # hash. Units are coded 1..U; times are matched against the observed period
  # grid (1..T), so gaps in the panel still resolve to "no such period" -> 0,
  # exactly as the previous string-keyed version did. The composite
  # `ug * span + tcode` is collision-free because tcode < span by construction.
  ug      <- match(u, unique(u))     # 1..U unit codes
  tlevels <- sort(unique(tm))        # observed period grid (handles gaps)
  tcode   <- match(tm, tlevels)      # 1..T period codes
  span    <- length(tlevels) + 1L    # stride between unit blocks

  src_key <- ug * span + tcode       # one integer key per row

  fetch_D <- function(offset) {
    # Shift in the time *value* domain, then map back onto the period grid;
    # periods that don't exist (panel edges / gaps) come back as NA -> 0.
    q_tcode <- match(tm - offset, tlevels)
    val <- trt[match(ug * span + q_tcode, src_key)]
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

  # Post periods (event times 0 .. leads): treatment lagged by h.
  for (h in 0:leads) {
    add_col(sprintf("nabs_dl_p%d", h), h, fetch_D(h))
  }

  # Pre periods (event times -2 .. -lags); -1 is the omitted reference.
  if (lags >= 2L) {
    for (h in 2:lags) {
      add_col(sprintf("nabs_dl_m%d", h), -h, fetch_D(-h))
    }
  }

  list(data = d, map = map)
}

# Collect the per-period distributed-lag coefficients into an event-study table.
#
# Reads each estimated column's coefficient and standard error directly (no
# cumulation), maps it to its event time, and appends the reference period -1
# with estimate 0 and standard error 0.
#
# Returns data.frame(time, estimate, std.error), ordered by time.
collect_dl <- function(estimate, se, map) {
  keep <- intersect(names(map), names(estimate))
  keep <- keep[!is.na(estimate[keep])]

  times <- unname(map[keep])
  est <- unname(estimate[keep])
  s <- unname(se[keep])

  # Reference period -1: estimate 0, SE 0.
  times <- c(times, -1L)
  est <- c(est, 0)
  s <- c(s, 0)

  ord <- order(times)
  data.frame(
    time = times[ord],
    estimate = est[ord],
    std.error = s[ord],
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
