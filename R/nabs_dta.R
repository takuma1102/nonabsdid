# Stata (.dta) interoperability helpers.
#
# Two exported functions:
#   * nabs_read_dta()  -- read a .dta file and return a plain panel data frame
#     that the estimator wrappers can digest (no haven_labelled columns, no
#     tagged missing values, unless explicitly requested).
#   * nabs_write_dta() -- write an nabs_event_study_tbl (or anything coercible
#     to one) back to a .dta file with Stata-valid variable names.
#
# Plus one internal helper, resolve_panel_data(), which lets the `data`
# argument of nabs_event_study() / nabs_event_study_simple() be a path to a
# .dta file directly.

#' Read a Stata .dta file into an analysis-ready data frame
#'
#' `nabs_read_dta()` is a thin convenience layer over [haven::read_dta()]
#' that smooths out the two places where freshly imported Stata data tends
#' to trip up R estimation packages:
#'
#' \itemize{
#'   \item **Labelled columns.** Stata value labels arrive in R as
#'     `haven_labelled` vectors, which many modeling functions (including
#'     the estimator packages wrapped by nonabsdid) do not understand. By
#'     default these are converted to factors; set `labelled = "numeric"`
#'     to drop the labels and keep the underlying codes instead.
#'   \item **Extended missing values.** Stata's `.a`--`.z` arrive as
#'     *tagged* `NA`s, which compare and print like ordinary `NA` but can
#'     survive into model matrices in surprising ways. By default all
#'     tagged `NA`s are collapsed to regular `NA`.
#' }
#'
#' Variable labels (Stata's `label variable`) are preserved as `"label"`
#' attributes on each column; they are harmless to the estimators and often
#' useful for plot labels.
#'
#' You rarely need to call this function yourself: [nabs_event_study()] and
#' [nabs_event_study_simple()] accept a path to a `.dta` file as their
#' `data` argument and route it through `nabs_read_dta()` automatically.
#'
#' @param path Path to a `.dta` file.
#' @param labelled How to handle `haven_labelled` columns. One of:
#'   \describe{
#'     \item{`"factor"` (default)}{Convert labelled columns to factors via
#'       [haven::as_factor()]. Unlabelled values keep their code as the
#'       level name.}
#'     \item{`"numeric"`}{Strip value labels via [haven::zap_labels()],
#'       keeping the underlying numeric codes. Use this when a labelled
#'       column is really a numeric variable (e.g. a 0/1 treatment dummy
#'       that happens to carry labels).}
#'     \item{`"keep"`}{Leave `haven_labelled` columns untouched. Note that
#'       the estimator packages may not accept them.}
#'   }
#' @param missings How to handle Stata extended missing values
#'   (`.a`--`.z`). `"na"` (default) collapses them to regular `NA` via
#'   [haven::zap_missing()]; `"keep"` preserves the tags.
#' @param encoding Passed to [haven::read_dta()]. Only needed for files
#'   written by Stata 13 or older with a non-default encoding.
#' @param verbose Logical; if `TRUE` (default), print a one-line summary of
#'   what was read and converted.
#' @param ... Additional arguments passed to [haven::read_dta()]
#'   (e.g. `col_select`, `n_max`).
#'
#' @return A tibble.
#'
#' @seealso [nabs_write_dta()] for the reverse direction, and the
#'   "nonabsdid for Stata users" vignette
#'   (`vignette("nonabsdid-for-stata-users")`) for a full Stata-to-R
#'   walk-through.
#'
#' @examples
#' if (requireNamespace("haven", quietly = TRUE)) {
#'   # Round-trip a small labelled panel through a temporary .dta file.
#'   tmp <- tempfile(fileext = ".dta")
#'   panel <- data.frame(id = rep(1:3, each = 2), t = rep(1:2, 3),
#'                       d = c(0, 1, 0, 0, 1, 1),
#'                       y = rnorm(6))
#'   haven::write_dta(panel, tmp)
#'
#'   mydata <- nabs_read_dta(tmp)
#'   head(mydata)
#' }
#' @export
nabs_read_dta <- function(path,
                          labelled = c("factor", "numeric", "keep"),
                          missings = c("na", "keep"),
                          encoding = NULL,
                          verbose = TRUE,
                          ...) {
  rlang::check_installed("haven", reason = "to read Stata .dta files.")
  labelled <- match.arg(labelled)
  missings <- match.arg(missings)

  if (!is.character(path) || length(path) != 1L) {
    cli::cli_abort("{.arg path} must be a single file path.")
  }
  if (!file.exists(path)) {
    cli::cli_abort("File {.file {path}} does not exist.")
  }

  out <- haven::read_dta(path, encoding = encoding, ...)

  # --- extended missing values (.a-.z arrive as tagged NA) -------------------
  # zap_missing() clears missing *labels* on labelled vectors, but does NOT
  # strip tags from a plain double that carries tagged NAs directly (the
  # common case for a numeric column read from a .dta). So replace tagged
  # elements with regular NA explicitly; this is robust to column type and
  # to haven's storage details.
  has_tagged <- function(col) {
    if (!is.double(col)) return(FALSE)
    isTRUE(any(haven::is_tagged_na(col)))
  }
  tagged_flags <- vapply(out, has_tagged, logical(1L))
  n_tagged_cols <- sum(tagged_flags)
  if (missings == "na" && n_tagged_cols > 0L) {
    for (j in which(tagged_flags)) {
      col <- out[[j]]
      col[haven::is_tagged_na(col)] <- NA_real_
      out[[j]] <- col
    }
  }

  # --- value-labelled columns -------------------------------------------------
  labelled_cols <- names(out)[vapply(
    out, inherits, logical(1L), what = "haven_labelled"
  )]
  if (length(labelled_cols) > 0L) {
    out <- switch(
      labelled,
      factor  = haven::as_factor(out, only_labelled = TRUE),
      numeric = haven::zap_labels(out),
      keep    = out
    )
  }

  if (isTRUE(verbose)) {
    cli::cli_inform(c(
      "Read {.file {path}}: {nrow(out)} row{?s}, {ncol(out)} column{?s}.",
      if (length(labelled_cols) > 0L && labelled != "keep") {
        stats::setNames(
          paste0("Converted {length(labelled_cols)} value-labelled ",
                 "column{?s} ({.field {labelled_cols}}) to ",
                 if (labelled == "factor") "factor." else "numeric codes."),
          "i"
        )
      },
      if (length(labelled_cols) > 0L && labelled == "keep") {
        c("!" = paste0("Kept {length(labelled_cols)} {.cls haven_labelled} ",
                       "column{?s} ({.field {labelled_cols}}); the estimator ",
                       "packages may not accept them."))
      },
      if (n_tagged_cols > 0L && missings == "na") {
        c("i" = paste0("Collapsed Stata extended missing values (.a\u2013.z) ",
                       "to {.val NA} in {n_tagged_cols} column{?s}."))
      },
      if (n_tagged_cols > 0L && missings == "keep") {
        c("!" = paste0("Kept tagged missing values in {n_tagged_cols} ",
                       "column{?s}."))
      }
    ))
  }

  out
}

#' Write event-study results to a Stata .dta file
#'
#' `nabs_write_dta()` exports an `nabs_event_study_tbl` -- or anything that
#' [as_nabs_event_study()] can coerce into one, including the result objects
#' returned by [nabs_event_study()] and [nabs_event_study_simple()] -- to a
#' Stata `.dta` file via [haven::write_dta()].
#'
#' The tidy schema uses dots in some column names (`std.error`, `conf.low`,
#' `conf.high`), which are not valid Stata variable names. These are renamed
#' to underscore versions (`std_error`, `conf_low`, `conf_high`) on the way
#' out; any other invalid characters are likewise replaced with `_`.
#'
#' This makes the "estimate in R, post-process in Stata" workflow a
#' one-liner: a Stata-using coauthor can rebuild the event-study figure with
#' `twoway rcap`/`scatter`, or feed the estimates into their own tables.
#'
#' @param x An `nabs_event_study_tbl`, an `nabs_event_study_result`, an
#'   `nabs_event_study_simple`, a supported estimator object, or a plain
#'   data frame with at least `time` and `estimate` columns. Anything that
#'   is not already a data frame is routed through [as_nabs_event_study()].
#' @param path Path of the `.dta` file to write.
#' @param version Stata file format version, passed to
#'   [haven::write_dta()]. Default `14` (readable by Stata 14 and later).
#' @param label Optional dataset label (Stata's `label data`), passed to
#'   [haven::write_dta()].
#' @param verbose Logical; if `TRUE` (default), print a one-line summary
#'   including any column renames.
#'
#' @return The path, invisibly.
#'
#' @seealso [nabs_read_dta()] for the reverse direction.
#'
#' @examples
#' if (requireNamespace("haven", quietly = TRUE)) {
#'   tidy <- as_nabs_event_study(
#'     data.frame(time = -2:3,
#'                estimate = c(0.02, -0.01, 0, 0.4, 0.5, 0.45),
#'                std.error = 0.1),
#'     method = "DCDH", outcome = "y"
#'   )
#'   tmp <- tempfile(fileext = ".dta")
#'   nabs_write_dta(tidy, tmp)
#'
#'   haven::read_dta(tmp)
#' }
#' @export
nabs_write_dta <- function(x, path, version = 14, label = NULL,
                           verbose = TRUE) {
  rlang::check_installed("haven", reason = "to write Stata .dta files.")

  if (!is.character(path) || length(path) != 1L) {
    cli::cli_abort("{.arg path} must be a single file path.")
  }

  out <- if (is.data.frame(x)) x else as_nabs_event_study(x)
  out <- as.data.frame(out, stringsAsFactors = FALSE)

  # --- make variable names Stata-valid ---------------------------------------
  # Stata names: letters, digits, underscores; must not start with a digit;
  # at most 32 characters.
  old <- names(out)
  new <- gsub("[^A-Za-z0-9_]", "_", old)
  starts_bad <- grepl("^[0-9]", new)
  new[starts_bad] <- paste0("v", new[starts_bad])
  new <- substr(new, 1L, 32L)
  new <- make.unique(new, sep = "_")
  renamed <- old != new
  names(out) <- new

  haven::write_dta(out, path, version = version, label = label)

  if (isTRUE(verbose)) {
    ren <- paste0("{.field ", old[renamed], "} -> {.field ", new[renamed], "}")
    cli::cli_inform(c(
      "Wrote {nrow(out)} row{?s} to {.file {path}} (Stata version {version}).",
      if (any(renamed)) {
        c("i" = paste0("Renamed for Stata: ", toString(ren), "."))
      }
    ))
  }

  invisible(path)
}

# Internal: allow `data` arguments to be a path to a .dta file. Used by
# nabs_event_study() and nabs_event_study_simple().
resolve_panel_data <- function(data, arg = "data") {
  if (is.character(data) && length(data) == 1L &&
      grepl("\\.dta$", data, ignore.case = TRUE)) {
    cli::cli_inform(
      "Reading {.arg {arg}} from Stata file {.file {data}} via {.fun nabs_read_dta}."
    )
    return(nabs_read_dta(data, verbose = FALSE))
  }
  data
}
