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
#' @param data A panel data frame.
#' @param outcome,treatment,unit,time Character column names.
#' @param method One of `"DCDH"`, `"PanelMatch"`, `"IFE"`.
#' @param lags,leads Integer pre- and post-period lengths.
#' @param controls Optional character vector of covariate names.
#' @param cluster Character; cluster variable. Defaults to `unit`.
#' @param conf.level Confidence level for the tidied output. Default 0.95.
#' @param ... Extra arguments passed straight to the underlying estimator.
#'
#' @return A list of class `"nabs_event_study_result"` with elements:
#'   \describe{
#'     \item{`tidy`}{An `nabs_event_study_tbl`.}
#'     \item{`fit`}{The native estimator object (for diagnostics).}
#'     \item{`call`}{The call that produced it.}
#'   }
#'
#' @examples
#' \dontrun{
#'   set.seed(1)
#'   panel <- expand.grid(id = 1:40, t = 1:10)
#'   panel$d <- rbinom(nrow(panel), 1, 0.3)
#'   panel$y <- 0.4 * panel$d + rnorm(nrow(panel))
#'   res_dcdh <- nabs_event_study(panel, outcome = "y", treatment = "d",
#'                                unit = "id", time = "t",
#'                                method = "DCDH",
#'                                lags = 2, leads = 3)
#'   res_dcdh$tidy
#' }
#' @export
nabs_event_study <- function(data, outcome, treatment, unit, time,
                        method = c("DCDH", "PanelMatch", "IFE", "FE", "MC"),
                        lags = 6L, leads = 8L,
                        controls = NULL,
                        cluster = unit,
                        conf.level = 0.95,
                        ...) {
  method <- match.arg(method)
  call <- match.call()

  fit <- switch(
    method,
    DCDH       = run_dcdh(data, outcome, treatment, unit, time,
                          lags, leads, controls, cluster, ...),
    PanelMatch = run_panelmatch(data, outcome, treatment, unit, time,
                                lags, leads, controls, ...),
    IFE        = run_fect(data, outcome, treatment, unit, time,
                          controls, fect_method = "ife", ...),
    FE         = run_fect(data, outcome, treatment, unit, time,
                          controls, fect_method = "fe", ...),
    MC         = run_fect(data, outcome, treatment, unit, time,
                          controls, fect_method = "mc", ...)
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
# and `placebo` by inspecting the *unevaluated* argument expression, not its
# value. A bare numeric literal works; a variable or an arithmetic expression
# is rejected even when it evaluates to a valid positive integer:
#
#     "Syntax error in effects option. Positive integer required."
#
#   did_multiplegt_dyn(..., effects = 5)          # OK    (numeric literal)
#   did_multiplegt_dyn(..., effects = leads + 1L) # FAILS (expression)
#   did_multiplegt_dyn(..., effects = my_var)     # FAILS (variable)
#
# The previous run_dcdh() passed `effects = leads + 1L` and `placebo = lags`
# (an expression and a variable), so the DCDH path failed at RUNTIME on such
# versions. R CMD check did not catch it: the @examples DCDH call is in
# \dontrun{} and tests use skip_if_not_installed("DIDmultiplegtDYN"), so the
# offending line is never evaluated; and `leads + 1L` is syntactically valid R,
# so static checks stay green. The error only surfaces when the package's NSE
# validator runs.
#
# FIX: never hand the package an expression or a symbol. We resolve the window
# to plain integers and dispatch with do.call(), which splices the numeric
# VALUES into the call it builds -- so the package's match.call()/substitute()
# sees numeric constants, exactly like a hand-written literal.
#
# (If a future version still rejects do.call'd numerics, switch to building the
# call from a parsed string where effects/placebo are written as literals:
#   expr <- parse(text = sprintf("f(..., effects = %d, placebo = %d)", eff, pl))
# Parsing guarantees literal constants no matter how the package introspects.)
run_dcdh <- function(data, outcome, treatment, unit, time,
                     lags, leads, controls, cluster, ...) {
  rlang::check_installed("DIDmultiplegtDYN", reason = "to fit DCDH estimators.")
 
  # +1: the tidier shifts DCDH's axis left by one (native reference at 0, ours
  # at -1), so native Effect_(leads+1) is what lands at x = +leads. Resolve to a
  # plain integer *here* (not in the call) so the call carries a value, not an
  # expression.
  eff <- as.integer(leads) + 1L
  pl  <- as.integer(lags)
 
  if (is.na(eff) || eff <= 0L) {
    cli::cli_abort("DCDH needs a positive integer {.arg leads} (got {.val {leads}}).")
  }
  if (is.na(pl) || pl < 0L) {
    cli::cli_abort("DCDH needs a non-negative integer {.arg lags} (got {.val {lags}}).")
  }
 
  # do.call() builds the call with the numeric VALUES of eff/pl spliced in, so
  # the NSE validator inside did_multiplegt_dyn sees literals, not `eff`/`pl`.
  do.call(
    DIDmultiplegtDYN::did_multiplegt_dyn,
    c(
      list(
        df        = as.data.frame(data),
        outcome   = outcome,
        group     = unit,
        time      = time,
        treatment = treatment,
        cluster   = cluster,
        controls  = controls
      ),
      list(effects = eff, placebo = pl),
      list(...)
    )
  )
}

run_panelmatch <- function(data, outcome, treatment, unit, time,
                           lags, leads, controls, ...) {
  rlang::check_installed("PanelMatch", reason = "to fit PanelMatch estimators.")

  pd <- PanelMatch::PanelData(
    panel.data = as.data.frame(data),
    unit.id    = unit,
    time.id    = time,
    treatment  = treatment,
    outcome    = outcome
  )

  # `ps.match` builds a propensity score, which needs covariates. With no
  # controls there is nothing to model it on, so PanelMatch produces an empty
  # refinement and downstream code errors with "attempt to set an attribute
  # on NULL". Fall back to exact matching on treatment history ("none") then.
  has_controls <- length(controls) > 0L
  covs_fml <- if (has_controls) stats::reformulate(controls) else NULL
  refine   <- if (has_controls) "ps.match" else "none"

  pm <- PanelMatch::PanelMatch(
    panel.data = pd,
    lag        = lags,
    refinement.method = refine,
    match.missing = TRUE,
    covs.formula  = covs_fml,
    size.match    = 10L,
    qoi           = "att",
    lead          = 0:leads,
    forbid.treatment.reversal = FALSE,
    placebo.test  = TRUE,
    ...
  )

  pe <- PanelMatch::PanelEstimate(sets = pm, panel.data = pd,
                                  se.method = "bootstrap")
  pl <- PanelMatch::placebo_test(pm.obj = pm, panel.data = pd,
                                 plot = FALSE, se.method = "bootstrap")
  list(pe = pe, placebo = pl, pm = pm, panel = pd)
}

run_fect <- function(data, outcome, treatment, unit, time, controls,
                     fect_method = c("ife", "fe", "mc"), ...) {
  fect_method <- match.arg(fect_method)
  rlang::check_installed(
    "fect",
    reason = sprintf("to fit the %s estimator (via fect::fect()).",
                     toupper(fect_method))
  )

  rhs <- if (length(controls)) {
    paste(treatment, "+", paste(controls, collapse = " + "))
  } else treatment
  fml <- stats::as.formula(paste(outcome, "~", rhs))

  # CV is meaningful for IFE (selects number of factors) and MC (selects
  # tuning parameter); for plain FE there's nothing to cross-validate, so
  # we turn it off to avoid spurious warnings.
  use_cv <- fect_method %in% c("ife", "mc")

  fect::fect(
    formula = fml,
    data    = as.data.frame(data),
    index   = c(unit, time),
    force   = "two-way",
    method  = fect_method,
    CV      = use_cv,
    se      = TRUE,
    nboots  = 200L,
    na.rm   = TRUE,
    ...
  )
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
