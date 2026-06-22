#' @rdname as_nabs_effect_cells
#'
#' @details
#' ## DCDH method
#'
#' Expects a `did_multiplegt_dyn` object **run with the `by` option**, where the
#' `by` variable is a unit-level onset cohort (e.g. each unit's first treated
#' period). When `by` is set, the object is reshaped into one sublist per `by`
#' level, each carrying its own event-study `plot$data` (`Time`, `Estimate`,
#' `LB.CI`, `UB.CI`, and sometimes `SE`). This method walks those sublists and
#' stacks them into the cohort-by-time schema, shifting the axis so onset sits
#' at `event_time = 0` (the same `-1` shift the event-study tidier applies).
#'
#' Building the cohort `by` variable and running DCDH for you is exactly what
#' [nabs_effect_cells()] with `method = "DCDH"` does; call the generic directly
#' only when you already have a `by`-run object in hand.
#'
#' SEs are the estimator's own (`se_method = "native"`) when `Time`-level SEs
#' are present in the plot data; otherwise CIs are carried through and the SE
#' column is `NA`.
#'
#' @export
as_nabs_effect_cells.did_multiplegt_dyn <- function(x, method = NULL,
                                                    outcome = NA_character_,
                                                    conf.level = 0.95, ...) {
  levels_found <- collect_dcdh_by_levels(x)
  if (!length(levels_found)) {
    cli::cli_abort(c(
      "This {.cls did_multiplegt_dyn} object has no per-cohort sublists.",
      "i" = "Run {.fun DIDmultiplegtDYN::did_multiplegt_dyn} with a unit-level \\
             cohort {.arg by} variable, or use {.fun nabs_effect_cells}."
    ))
  }

  parts <- lapply(levels_found, function(lv) {
    pd <- lv$data
    has_se <- "SE" %in% names(pd)
    new_effect_cell_tbl(
      cohort        = rep(lv$cohort, nrow(pd)),
      event_time    = as.integer(pd$Time) - 1L,   # native ref at 0 -> ours at -1
      estimate      = pd$Estimate,
      std.error     = if (has_se) pd$SE else NA_real_,
      conf.low      = pd[["LB.CI"]],
      conf.high     = pd[["UB.CI"]],
      calendar_time = lv$cohort + (as.integer(pd$Time) - 1L),
      method        = method %||% "DCDH",
      outcome       = outcome,
      se_method     = if (has_se) "native" else "none",
      conf.level    = conf.level
    )
  })

  out <- bind_effect_cells(parts)
  attr(out, "conf.level") <- conf.level
  out
}

# Recursively locate by-level sublists: any list element that itself holds a
# `plot$data` frame with Time/Estimate, excluding the top-level combined plot.
# Returns a list of list(cohort = <int>, data = <df>). Best-effort: cohort
# labels come from the list names, parsed to numeric where possible.
collect_dcdh_by_levels <- function(x) {
  out <- list()
  is_es_data <- function(d) {
    is.data.frame(d) && all(c("Time", "Estimate") %in% names(d))
  }
  walk <- function(node, name, depth) {
    if (depth > 3L || !is.list(node)) return(invisible())
    # A by-level node carries its own plot$data but is not the top-level object.
    pd <- tryCatch(node[["plot"]][["data"]], error = function(e) NULL)
    if (depth >= 1L && is_es_data(pd)) {
      out[[length(out) + 1L]] <<- list(cohort = parse_cohort_label(name),
                                       data = pd)
      return(invisible())
    }
    nms <- names(node)
    for (i in seq_along(node)) {
      el <- node[[i]]
      nm <- if (!is.null(nms)) nms[i] else NA_character_
      if (identical(nm, "args") || identical(nm, "plot")) next
      if (is.list(el)) walk(el, nm, depth + 1L)
    }
    invisible()
  }
  walk(x, NA_character_, 0L)

  # Drop any entry whose cohort label could not be parsed to a number; a cohort
  # axis must be numeric for the heatmap to order correctly.
  out <- Filter(function(e) !is.na(e$cohort), out)
  out
}

# By-level names from DIDmultiplegtDYN look like "<var> = 4" or just "4";
# pull the trailing number.
parse_cohort_label <- function(nm) {
  if (is.null(nm) || is.na(nm) || !nzchar(nm)) return(NA_integer_)
  m <- regmatches(nm, regexpr("-?[0-9]+(\\.[0-9]+)?", nm))
  if (!length(m)) return(NA_integer_)
  suppressWarnings(as.integer(round(as.numeric(m))))
}
