#' Run an event-study estimator with a unified interface
#'
#' `nabs_event_study()` is a thin wrapper around the three supported estimators
#' (DCDH, PanelMatch, IFE/fect) that takes a single, common argument set
#' and dispatches to the correct underlying package. It is **not** intended
#' to expose every option of every estimator; for that, call the underlying
#' packages directly and tidy their output with [as_nabs_event_study()].
#'
#' What it does cover:
#' \itemize{
#'   \item Variable names (outcome, treatment, unit, time),
#'   \item Pre/post window length (`lags`, `leads`),
#'   \item Optional covariates and clustering,
#'   \item Reasonable defaults that match the three packages' typical use.
#' }
#'
#' @param data A panel data frame, or a path to a Stata `.dta` file (which
#'   is read via [nabs_read_dta()] with default settings).
#' @param outcome,treatment,unit,time Character column names.
#' @param method One of `"DCDH"`, `"PanelMatch"`, `"IFE"`.
#' @param lags,leads Integer pre- and post-period lengths.
#' @param controls Optional character vector of covariate names.
#' @param cluster Character; cluster variable. Defaults to `unit`.
#' @param conf.level Confidence level for the tidied output. Default 0.95.
#' @param cv,nboots,r,parallel,cores Tuning knobs for the `fect` family
#'   (`IFE`, `FE`, `MC`); ignored by other methods. `cv` toggles
#'   cross-validation (default: on for `IFE`/`MC`, off for `FE`); `r` caps /
#'   fixes the number of interactive-fixed-effect factors; `nboots` is the
#'   bootstrap count (default 200). `parallel` defaults to `FALSE` because, on
#'   large panels, copying the data to parallel workers tends to exhaust memory
#'   rather than help; set `parallel = TRUE` (optionally with `cores`) for big
#'   speedups on small panels. These are first-class arguments so that, e.g.,
#'   `cv = FALSE` no longer collides with internal defaults.
#' @param k,nlambda,vartype,se Further `fect`-family speed knobs. `k` is the
#'   number of cross-validation rounds; `fect`'s own default is 20, which is
#'   slow on large panels, so the wrapper defaults it to 5 when CV is on.
#'   `nlambda` caps the MC regularisation grid (wrapper default 5 vs `fect`'s
#'   10). `vartype` selects the variance estimator (`"bootstrap"`,
#'   `"jackknife"`, or `"parametric"`); `"parametric"` is available for `IFE`
#'   and avoids refitting the factor model on every resample, but is not
#'   supported for `MC`. `se = FALSE` skips uncertainty entirely for a fast
#'   point-estimate-only pass. Advanced `fect` knobs (`tol`, `max.iteration`,
#'   `em`, `lambda`) may also be passed through `...`.
#' @param number.iterations,se.method,run_placebo,num.cores Tuning knobs for
#'   `PanelMatch`; ignored by other methods. `number.iterations` is the
#'   bootstrap count (default 1000); lower it (e.g. 200) for tractability.
#'   `se.method` selects the SE type (`"bootstrap"`, `"conditional"`,
#'   `"unconditional"`); the analytic `"conditional"`/`"unconditional"` methods
#'   skip the bootstrap entirely and are by far the biggest speed-up.
#'   `run_placebo = FALSE` skips the separate placebo-test bootstrap (a second
#'   full bootstrap pass). `parallel`/`num.cores` are forwarded to
#'   `PanelEstimate()` to spread the bootstrap across cores.
#' @param ... Extra arguments passed straight to the underlying estimator.
#'   Stata-style aliases are also accepted here and translated with an
#'   informative message: `df` (for `data`), `group` (for `unit`),
#'   `placebo` (for `lags`), and `effects` (for `leads`; note
#'   `leads = effects - 1`, because nonabsdid places treatment onset at
#'   relative time 0). See the "nonabsdid for Stata users" vignette.
#'
#' @return A list of class `"nabs_event_study_result"` with elements:
#'   \describe{
#'     \item{`tidy`}{An `nabs_event_study_tbl`.}
#'     \item{`fit`}{The native estimator object (for diagnostics).}
#'     \item{`call`}{The call that produced it.}
#'   }
#'
#' @examples
#'  if (requireNamespace("DIDmultiplegtDYN", quietly = TRUE) &&
#'      requireNamespace("polars", quietly = TRUE)) {
#'   set.seed(1)
#'   library(polars)
#'   panel <- expand.grid(id = 1:60, t = 1:10)
#'   panel$d <- with(panel, as.integer(
#'     (id %% 4 == 1 & t %in% 4:7) |
#'     (id %% 4 == 2 & t %in% 5:8) |
#'     (id %% 4 == 3 & t %in% 6:9)
#'   ))
#'   panel$y <- 0.2 * panel$t + 0.5 * panel$d + rnorm(nrow(panel))
#'
#'   res_dcdh <- nabs_event_study(
#'     panel,
#'     outcome = "y",
#'     treatment = "d",
#'     unit = "id",
#'     time = "t",
#'     method = "DCDH",
#'     lags = 2,
#'     leads = 2
#'   )
#'   res_dcdh$tidy
#' }
#' @export
nabs_event_study <- function(data, outcome, treatment, unit, time,
                        method = c("DCDH", "PanelMatch", "IFE", "FE", "MC"),
                        lags = 6L, leads = 8L,
                        controls = NULL,
                        cluster = unit,
                        conf.level = 0.95,
                        cv = NULL, nboots = NULL, r = NULL,
                        k = NULL, nlambda = NULL, vartype = NULL, se = NULL,
                        parallel = FALSE, cores = NULL,
                        number.iterations = NULL, se.method = NULL,
                        run_placebo = NULL, num.cores = NULL,
                        ...) {
  method <- match.arg(method)
  call <- match.call()

  # Stata-style aliases (df/group/effects/placebo) supplied through `...`
  # are translated onto the canonical arguments; see translate_stata_dots().
  st <- translate_stata_dots(
    list(...),
    have = list(
      data  = !missing(data),
      unit  = !missing(unit),
      lags  = !missing(lags),
      leads = !missing(leads)
    )
  )
  if (!is.null(st$values$data))  data  <- st$values$data
  if (!is.null(st$values$unit))  unit  <- st$values$unit
  if (!is.null(st$values$lags))  lags  <- st$values$lags
  if (!is.null(st$values$leads)) leads <- st$values$leads
  dots <- st$dots

  # `data` may also be a path to a .dta file.
  data <- resolve_panel_data(data)

  # Guard layer: validate the panel and coerce ids where an estimator needs it
  # (character unit -> integer for PanelMatch; string cluster -> integer for
  # DCDH's polars backend). `unit`/`cluster`/`controls` may be renamed here.
  pf <- preflight_panel(
    data, outcome = outcome, treatment = treatment,
    unit = unit, time = time, controls = controls, cluster = cluster
  )
  data     <- pf$data
  unit     <- pf$unit
  cluster  <- pf$cluster
  controls <- pf$controls

  # User-supplied tuning knobs, forwarded only to the estimators that use them.
  fect_knobs <- drop_nulls(list(
    cv = cv, nboots = nboots, r = r, k = k, nlambda = nlambda,
    vartype = vartype, se = se, parallel = parallel, cores = cores
  ))
  pm_knobs <- drop_nulls(list(
    number.iterations = number.iterations, se.method = se.method,
    run_placebo = run_placebo, parallel = parallel, num.cores = num.cores
  ))

  fit <- switch(
    method,
    DCDH       = do.call(run_dcdh,
                         c(list(data, outcome, treatment, unit, time,
                                lags, leads, controls, cluster), dots)),
    PanelMatch = do.call(run_panelmatch,
                         c(list(data, outcome, treatment, unit, time,
                                lags, leads, controls), pm_knobs, dots)),
    IFE        = do.call(run_fect,
                         c(list(data, outcome, treatment, unit, time,
                                controls, fect_method = "ife"),
                           fect_knobs, dots)),
    FE         = do.call(run_fect,
                         c(list(data, outcome, treatment, unit, time,
                                controls, fect_method = "fe"),
                           fect_knobs, dots)),
    MC         = do.call(run_fect,
                         c(list(data, outcome, treatment, unit, time,
                                controls, fect_method = "mc"),
                           fect_knobs, dots))
  )

  tidy <- if (method == "PanelMatch") {
    as_nabs_event_study(fit$pe, pre_obj = fit$placebo,
                   method = "PanelMatch", outcome = outcome,
                   conf.level = conf.level)
  } else {
    as_nabs_event_study(fit, method = method, outcome = outcome,
                   conf.level = conf.level)
  }

  structure(
    list(tidy = tidy, fit = fit, call = call),
    class = "nabs_event_study_result"
  )
}

# ----- internal estimator runners --------------------------------------------
# ----------------------------------------------------------------------------
# IMPORTANT — DIDmultiplegtDYN's `effects` / `placebo` and non-standard eval
# ----------------------------------------------------------------------------
# VERIFIED (DIDmultiplegtDYN 2.3.3): did_multiplegt_dyn() validates `effects`
# and `placebo` by inspecting the *unevaluated* call (it re-reads its own call
# via match.call()/sys.call()), NOT the argument values. Only a bare numeric
# literal in the source call passes; a variable, an arithmetic expression, OR a
# value spliced in through do.call() all fail -- even when the value is a valid
# positive integer:
#
#     "Syntax error in effects option. Positive integer required."
#
#   did_multiplegt_dyn(..., effects = 5)            # OK    (source literal)  [verified]
#   did_multiplegt_dyn(..., effects = leads + 1L)   # FAILS (expression)      [verified]
#   do.call(did_multiplegt_dyn, list(effects = 5L)) # FAILS (do.call splice)  [verified]
#
# FIX (works because it reproduces the verified-good form exactly): build the
# call from a STRING in which effects/placebo appear as bare numeric literals,
# then parse() + eval() it. After parsing, the call object holds literal numeric
# constants for effects/placebo -- byte-for-byte identical to a hand-typed
# `effects = 5`. All other arguments are NOT subject to NSE, so they are passed
# by reference via a dedicated evaluation environment (no need to inline them).
run_dcdh <- function(data, outcome, treatment, unit, time,
                     lags, leads, controls, cluster, ...) {
  rlang::check_installed("DIDmultiplegtDYN", reason = "to fit DCDH estimators.")
  # DIDmultiplegtDYN's backend refers to the bare symbol `pl`; make sure polars
  # is attached so it doesn't fail with "object 'pl' not found".
  ensure_polars()

  # The wrapper hands us a plain data.frame (preflight_panel() already
  # coerced); only copy if a direct caller passed something else, so we don't
  # duplicate a multi-GB panel on the normal path.
  if (!is.data.frame(data)) data <- as.data.frame(data)

  # +1: the tidier shifts DCDH's axis left by one (native reference at 0, ours
  # at -1), so native Effect_(leads+1) lands at x = +leads.
  eff <- as.integer(leads) + 1L
  pl  <- as.integer(lags)

  if (is.na(eff) || eff <= 0L) {
    cli::cli_abort("DCDH needs a positive integer {.arg leads} (got {.val {leads}}).")
  }
  if (is.na(pl) || pl < 0L) {
    cli::cli_abort("DCDH needs a non-negative integer {.arg lags} (got {.val {lags}}).")
  }

  # Evaluation environment: child of the package namespace (so
  # `did_multiplegt_dyn` and internals resolve), holding our by-reference args.
  env <- new.env(parent = asNamespace("DIDmultiplegtDYN"))
  env$.df        <- data
  env$.outcome   <- outcome
  env$.group     <- unit
  env$.time      <- time
  env$.treatment <- treatment
  env$.cluster   <- cluster
  env$.controls  <- controls

  # Named extra args (...). They are ordinary value arguments (not NSE), so we
  # bind each into `env` under its own name and reference it by name in the call
  # string. Unnamed dots are not supported (DCDH args are all named); error
  # clearly if any are unnamed.
  dots <- list(...)
  extra_src <- ""
  if (length(dots)) {
    nm <- names(dots)
    if (is.null(nm) || !all(nzchar(nm))) {
      cli::cli_abort("Extra arguments to DCDH must be named.")
    }
    for (k in seq_along(dots)) {
      vn <- paste0(".extra_", k)
      assign(vn, dots[[k]], envir = env)
      extra_src <- paste0(extra_src, sprintf(", %s = %s", nm[k], vn))
    }
  }

  # Build the call with effects/placebo as BARE LITERALS in the source text.
  code <- sprintf(
    "did_multiplegt_dyn(df = .df, outcome = .outcome, group = .group,
       time = .time, treatment = .treatment, cluster = .cluster,
       controls = .controls, effects = %d, placebo = %d%s)",
    eff, pl, extra_src
  )

  expr <- parse(text = code)[[1]]
  eval(expr, envir = env)
}

run_panelmatch <- function(data, outcome, treatment, unit, time,
                           lags, leads, controls,
                           number.iterations = 1000L,
                           se.method = "bootstrap",
                           refinement.method = NULL,
                           size.match = 10L,
                           match.missing = TRUE,
                           run_placebo = TRUE,
                           parallel = FALSE,
                           num.cores = 1L, ...) {
  rlang::check_installed("PanelMatch", reason = "to fit PanelMatch estimators.")

  # Already a plain data.frame on the wrapper path; only copy for direct callers.
  if (!is.data.frame(data)) data <- as.data.frame(data)

  pd <- PanelMatch::PanelData(
    panel.data = data,
    unit.id    = unit,
    time.id    = time,
    treatment  = treatment,
    outcome    = outcome
  )

  # `ps.match` builds a propensity score, which needs covariates. With no
  # controls there is nothing to model it on, so PanelMatch produces an empty
  # refinement and downstream code errors with "attempt to set an attribute
  # on NULL". Fall back to exact matching on treatment history ("none") then.
  # `refinement.method` may be set explicitly to override this auto choice.
  has_controls <- length(controls) > 0L
  covs_fml <- if (has_controls) stats::reformulate(controls) else NULL
  if (is.null(refinement.method)) {
    refinement.method <- if (has_controls) "ps.match" else "none"
  }

  pm <- PanelMatch::PanelMatch(
    panel.data = pd,
    lag        = lags,
    refinement.method = refinement.method,
    match.missing = isTRUE(match.missing),
    covs.formula  = covs_fml,
    size.match    = as.integer(size.match),
    qoi           = "att",
    lead          = 0:leads,
    forbid.treatment.reversal = FALSE,
    placebo.test  = isTRUE(run_placebo),
    ...
  )

  # Bootstrap SEs are the expensive part on large panels. Levers, in rough
  # order of impact:
  #   * se.method = "conditional"/"unconditional" -> analytic SEs, no bootstrap
  #     (available for qoi = "att"); by far the biggest speed-up.
  #   * run_placebo = FALSE -> skip the *second* (placebo) bootstrap pass.
  #   * parallel / num.cores -> spread the bootstrap across cores.
  #   * number.iterations -> fewer reps when se.method = "bootstrap".
  ni <- as.integer(number.iterations)
  pe <- PanelMatch::PanelEstimate(sets = pm, panel.data = pd,
                                  se.method = se.method,
                                  number.iterations = ni,
                                  parallel = isTRUE(parallel),
                                  num.cores = as.integer(num.cores))
  pl <- if (isTRUE(run_placebo)) {
    PanelMatch::placebo_test(pm.obj = pm, panel.data = pd,
                             plot = FALSE, se.method = se.method,
                             number.iterations = ni)
  } else NULL
  list(pe = pe, placebo = pl, pm = pm, panel = pd)
}

run_fect <- function(data, outcome, treatment, unit, time, controls,
                     fect_method = c("ife", "fe", "mc"),
                     cv = NULL, nboots = 200L, r = NULL,
                     k = NULL, nlambda = NULL, lambda = NULL,
                     vartype = NULL, se = TRUE,
                     tol = NULL, max.iteration = NULL, em = NULL,
                     parallel = FALSE, cores = NULL, ...) {
  fect_method <- match.arg(fect_method)
  rlang::check_installed(
    "fect",
    reason = sprintf("to fit the %s estimator (via fect::fect()).",
                     toupper(fect_method))
  )

  # Already a plain data.frame on the wrapper path; only copy for direct callers.
  if (!is.data.frame(data)) data <- as.data.frame(data)

  rhs <- if (length(controls)) {
    paste(treatment, "+", paste(controls, collapse = " + "))
  } else treatment
  fml <- stats::as.formula(paste(outcome, "~", rhs))

  # CV is meaningful for IFE (selects number of factors) and MC (selects the
  # tuning parameter); for plain FE there's nothing to cross-validate. Honour
  # an explicit `cv`, otherwise default per method.
  use_cv <- if (is.null(cv)) fect_method %in% c("ife", "mc") else isTRUE(cv)

  # fect's own CV defaults are tuned for thoroughness (k = 20 rounds, plus a
  # wide factor / lambda grid), which is very slow on large panels. Use lighter
  # defaults here; override explicitly for final/publication runs.
  if (use_cv) {
    if (is.null(k))                               k       <- 5L   # fect default: 20
    if (is.null(r)       && fect_method == "ife") r       <- 2L   # cap 0:2 factor search
    if (is.null(nlambda) && fect_method == "mc")  nlambda <- 5L   # fect default: 10
  }

  # Inference. The nonparametric bootstrap refits the whole model on every
  # resample; for IFE the parametric bootstrap (vartype = "parametric") avoids
  # that and is far cheaper. It is NOT available for MC, so guard against it.
  if (is.null(vartype)) vartype <- "bootstrap"
  if (identical(vartype, "parametric") && fect_method == "mc") {
    cli::cli_abort(c(
      "{.val parametric} {.arg vartype} is not available for the MC estimator.",
      "i" = "Use {.val bootstrap} or {.val jackknife}; lower {.arg nboots}, set \\
             {.arg parallel = TRUE}, or {.arg se = FALSE} to speed MC up."
    ))
  }

  # parallel defaults to FALSE: on large panels fect copies the data (and a
  # >1GB closure) to each worker, which blows past future.globals.maxSize and
  # is slower in practice. Opt in for small panels.
  args <- list(
    formula  = fml,
    data     = data,
    index    = c(unit, time),
    force    = "two-way",
    method   = fect_method,
    CV       = use_cv,
    se       = isTRUE(se),
    vartype  = vartype,
    nboots   = as.integer(nboots),
    na.rm    = TRUE,
    parallel = isTRUE(parallel)
  )
  if (!is.null(cores))         args$cores         <- as.integer(cores)
  if (!is.null(r))             args$r             <- r
  if (use_cv && !is.null(k))   args$k             <- as.integer(k)
  if (!is.null(nlambda))       args$nlambda       <- as.integer(nlambda)
  if (!is.null(lambda))        args$lambda        <- lambda
  if (!is.null(tol))           args$tol           <- tol
  if (!is.null(max.iteration)) args$max.iteration <- as.integer(max.iteration)
  if (!is.null(em))            args$em            <- isTRUE(em)

  # Anything in ... that would duplicate a protected argument is dropped with a
  # clear note (so e.g. CV = FALSE via ... no longer triggers a duplicate-arg
  # error); use the dedicated arguments instead.
  dots  <- list(...)
  clash <- intersect(names(dots), names(args))
  if (length(clash)) {
    cli::cli_warn(c(
      "Ignoring fect argument{?s} {.arg {clash}} passed via {.arg ...}.",
      "i" = "Use {.arg cv}, {.arg nboots}, {.arg r}, {.arg k}, {.arg nlambda}, \\
             {.arg vartype}, {.arg se}, {.arg parallel}, or {.arg cores} instead."
    ))
    dots <- dots[setdiff(names(dots), names(args))]
  }

  do.call(fect::fect, c(args, dots))
}

#' @export
print.nabs_event_study_result <- function(x, ...) {
  cli::cli_h1("nabs_event_study_result")
  cli::cli_text("method: {.val {unique(x$tidy$method)}}")
  cli::cli_text("rows in tidy: {nrow(x$tidy)} (pre: \\
                {sum(x$tidy$window == 'pre')}, \\
                post: {sum(x$tidy$window == 'post')})")
  cli::cli_text("call:")
  cli::cli_verbatim(deparse(x$call))
  invisible(x)
}
