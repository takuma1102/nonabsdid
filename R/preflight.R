# Internal guard layer shared by the estimator entry points.
#
# The supported estimators fail in confusing, package-specific ways when the
# panel isn't shaped exactly how each one expects (character unit ids for
# PanelMatch, string clusters for DCDH's polars backend, polars not attached,
# all-NA controls, ...). preflight_panel() catches those up front, fixes the
# safe ones automatically, and turns the rest into actionable messages so the
# failure is explained in nonabsdid's terms rather than an upstream backtrace.

# Drop NULL entries from a list (used to forward only user-supplied knobs).
drop_nulls <- function(x) {
  x[!vapply(x, is.null, logical(1))]
}

# A column name based on `base` that does not already exist in `existing`.
make_unique_name <- function(base, existing) {
  nm <- base
  i <- 1L
  while (nm %in% existing) {
    nm <- paste0(base, "_", i)
    i <- i + 1L
  }
  nm
}

# Evaluate `expr` under a fixed RNG seed without disturbing the caller's
# global RNG state (dependency-free withr::with_seed()).
with_local_seed <- function(seed, expr) {
  has_old <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  if (has_old) {
    old <- get(".Random.seed", envir = globalenv(), inherits = FALSE)
    on.exit(assign(".Random.seed", old, envir = globalenv()), add = TRUE)
  } else {
    on.exit(
      if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
        rm(".Random.seed", envir = globalenv())
      },
      add = TRUE
    )
  }
  set.seed(seed)
  force(expr)
}

# Make sure the polars package is attached, because DIDmultiplegtDYN's backend
# refers to the bare symbol `pl` and fails with "object 'pl' not found" when
# polars is merely installed but not on the search path. We attach it for the
# user (with a one-time note) rather than make them discover `library(polars)`
# from a cryptic error.
ensure_polars <- function() {
  if ("package:polars" %in% search()) {
    return(invisible(TRUE))
  }
  if (!requireNamespace("polars", quietly = TRUE)) {
    cli::cli_abort(c(
      "The DCDH backend ({.pkg DIDmultiplegtDYN}) needs the {.pkg polars} package.",
      "i" = "Install it with \\
             {.code install.packages(\"polars\", repos = \"https://rpolars.r-universe.dev\")}."
    ))
  }
  ok <- tryCatch({
    attachNamespace("polars")
    TRUE
  }, error = function(e) FALSE)
  if (!ok) {
    cli::cli_abort(c(
      "The DCDH backend needs {.pkg polars} attached, but it could not be \\
       loaded automatically.",
      "i" = "Please run {.code library(polars)} once, then try again."
    ))
  }
  cli::cli_alert_info("Attached {.pkg polars} for the DCDH backend.")
  invisible(TRUE)
}

# Validate and (where safe) repair a panel before handing it to an estimator.
#
# Returns a list with the possibly-modified `data`, plus the column names to
# actually use downstream (`unit`, `cluster`, `controls`) -- these can differ
# from the inputs when ids were coerced to integer codes.
#
# @param coerce_ids If TRUE (default), non-numeric `unit` / `cluster` columns
#   are replaced by integer codes (added as new columns), since PanelMatch
#   requires a numeric unit id and DCDH's polars backend requires a numeric
#   cluster. Coercion only relabels ids and never changes estimates.
# @param quiet Suppress the informational (non-error) messages.
#
# @keywords internal
# @noRd
preflight_panel <- function(data, outcome, treatment, unit, time,
                            controls = NULL, cluster = NULL,
                            coerce_ids = TRUE, quiet = FALSE) {
  data <- as.data.frame(data)

  # 1. Required columns exist.
  named <- c(outcome = outcome, treatment = treatment,
             unit = unit, time = time)
  for (role in names(named)) {
    col <- named[[role]]
    if (!col %in% names(data)) {
      cli::cli_abort(
        "Column {.field {col}} (the {role} variable) was not found in {.arg data}."
      )
    }
  }
  if (length(controls)) {
    miss <- setdiff(controls, names(data))
    if (length(miss)) {
      cli::cli_abort(c(
        "Some {.arg controls} are not columns in {.arg data}:",
        "x" = "{.field {miss}}"
      ))
    }
  }
  if (!is.null(cluster) && !cluster %in% names(data)) {
    cli::cli_abort("Cluster column {.field {cluster}} was not found in {.arg data}.")
  }

  # 2. Treatment is a 0/1 (or FALSE/TRUE) indicator.
  tv <- data[[treatment]]
  if (is.logical(tv)) {
    ok <- TRUE   # logical is, by definition, a 0/1 indicator
  } else {
    uvals     <- unique(tv[!is.na(tv)])
    uvals_num <- suppressWarnings(as.numeric(as.character(uvals)))
    ok <- length(uvals) == 0L ||
      (!anyNA(uvals_num) && all(uvals_num %in% c(0, 1)))
  }
  if (!ok) {
    shown <- utils::head(sort(unique(as.character(uvals))), 5L)
    cli::cli_abort(c(
      "Treatment {.field {treatment}} must be a 0/1 (or FALSE/TRUE) indicator.",
      "i" = "Found value{?s}: {.val {shown}}{if (length(uvals) > 5L) ', ...' else ''}.",
      "i" = "Recode it to 0 (untreated) / 1 (treated) before estimating."
    ))
  }

  # 3. Coerce ids to integer codes where an estimator requires it. This only
  #    relabels units/clusters and so leaves every estimate unchanged.
  if (isTRUE(coerce_ids)) {
    if (!is.numeric(data[[unit]])) {
      new <- make_unique_name("nabs_unit_id", names(data))
      data[[new]] <- as.integer(factor(data[[unit]]))
      if (!quiet) {
        cli::cli_alert_info(
          "Unit id {.field {unit}} is not numeric; using integer codes in \\
           {.field {new}} (PanelMatch and the DCDH backend need a numeric id)."
        )
      }
      if (!is.null(cluster) && identical(cluster, unit)) cluster <- new
      unit <- new
    }
    if (!is.null(cluster) && !is.numeric(data[[cluster]])) {
      new <- make_unique_name("nabs_cluster_id", names(data))
      data[[new]] <- as.integer(factor(data[[cluster]]))
      if (!quiet) {
        cli::cli_alert_info(
          "Cluster {.field {cluster}} is not numeric; using integer codes in \\
           {.field {new}} (the DCDH polars backend cannot cluster on strings)."
        )
      }
      cluster <- new
    }
  }

  # 4. Controls that are entirely NA would silently break the estimators.
  if (length(controls)) {
    all_na <- controls[vapply(
      controls, function(cc) all(is.na(data[[cc]])), logical(1)
    )]
    if (length(all_na)) {
      cli::cli_abort(c(
        "These {.arg controls} are entirely {.val NA}:",
        "x" = "{.field {all_na}}",
        "i" = "Drop them or fix the data before estimating."
      ))
    }
  }

  # 5. Note partial missingness: estimators drop NA rows differently, so the
  #    effective sample (and thus the comparison) can differ across methods.
  na_cols <- unique(c(outcome, controls))
  n_na <- sum(!stats::complete.cases(data[, na_cols, drop = FALSE]))
  if (n_na > 0L && !quiet) {
    cli::cli_alert_info(
      "{.val {n_na}} row{?s} have missing outcome/controls; estimators drop \\
       these differently, so effective samples may differ across methods."
    )
  }

  list(data = data, unit = unit, cluster = cluster, controls = controls)
}
