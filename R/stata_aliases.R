# Stata-style argument aliases.
#
# The Stata (and R) command did_multiplegt_dyn uses a different vocabulary
# from nonabsdid for the same concepts:
#
#   Stata / DCDH name   nonabsdid name        translation
#   -----------------   ---------------       -----------------------------
#   df                  data                  identical
#   group               unit                  identical
#   placebo             lags                  identical (number of placebos)
#   effects             leads                 leads = effects - 1
#
# The effects/leads off-by-one comes from the axis convention: DCDH counts
# `effects` post-treatment estimates starting at native period 1, while
# nonabsdid puts treatment onset at relative time 0, so a window of `leads`
# produces estimates at 0, 1, ..., leads -- i.e. leads + 1 post-period
# estimates. `effects = k` in Stata therefore corresponds to
# `leads = k - 1` here, and reproduces the identical underlying call (see
# run_dcdh(), which sets effects = leads + 1).
#
# Users coming straight from a Stata script can paste their option values
# and get an informative translation message rather than an opaque error.

# Translate Stata-style names found in `dots` onto canonical values.
#
# @param dots Named list captured from `...`.
# @param have Named logical list saying which canonical arguments the user
#   already supplied (`data`, `unit`, `lags`, `leads`). Supplying both the
#   canonical name and its alias is an error.
# @return A list with elements:
#   * `values`: named list of resolved canonical values (only the ones an
#     alias provided; absent otherwise),
#   * `dots`: `dots` with the consumed aliases removed.
# @noRd
translate_stata_dots <- function(dots, have, quiet = FALSE) {
  values <- list()
  notes  <- character()

  has_alias <- function(nm) nm %in% names(dots)
  drop_alias <- function(nm) dots[names(dots) != nm]

  conflict <- function(alias, canonical) {
    cli::cli_abort(c(
      "Both {.arg {canonical}} and its Stata-style alias {.arg {alias}} were supplied.",
      "i" = "Use one or the other (they mean the same thing)."
    ), call = NULL)
  }

  # df -> data ---------------------------------------------------------------
  if (has_alias("df")) {
    if (isTRUE(have$data)) conflict("df", "data")
    values$data <- dots[["df"]]
    dots <- drop_alias("df")
    notes <- c(notes, "{.arg df} -> {.arg data}")
  }

  # group -> unit --------------------------------------------------------------
  if (has_alias("group")) {
    if (isTRUE(have$unit)) conflict("group", "unit")
    values$unit <- dots[["group"]]
    dots <- drop_alias("group")
    notes <- c(notes, "{.arg group} -> {.arg unit}")
  }

  # placebo -> lags -------------------------------------------------------------
  if (has_alias("placebo")) {
    if (isTRUE(have$lags)) conflict("placebo", "lags")
    pl <- dots[["placebo"]]
    if (!is.numeric(pl) || length(pl) != 1L || is.na(pl) || pl < 0) {
      cli::cli_abort(
        "{.arg placebo} must be a single non-negative number (got {.val {pl}}).",
        call = NULL
      )
    }
    values$lags <- as.integer(pl)
    dots <- drop_alias("placebo")
    notes <- c(notes,
               sprintf("{.arg placebo} = %d -> {.arg lags} = %d",
                       as.integer(pl), as.integer(pl)))
  }

  # effects -> leads = effects - 1 ----------------------------------------------
  if (has_alias("effects")) {
    if (isTRUE(have$leads)) conflict("effects", "leads")
    eff <- dots[["effects"]]
    if (!is.numeric(eff) || length(eff) != 1L || is.na(eff) || eff < 1) {
      cli::cli_abort(
        "{.arg effects} must be a single positive number (got {.val {eff}}).",
        call = NULL
      )
    }
    values$leads <- as.integer(eff) - 1L
    dots <- drop_alias("effects")
    notes <- c(notes,
               sprintf("{.arg effects} = %d -> {.arg leads} = %d",
                       as.integer(eff), as.integer(eff) - 1L))
  }

  if (length(notes) > 0L && !isTRUE(quiet)) {
    n_notes <- length(notes)
    cli::cli_inform(c(
      "Translated {cli::qty(n_notes)}Stata-style argument{?s}:",
      stats::setNames(notes, rep("*", length(notes))),
      if (!is.null(values$leads)) {
        c("i" = paste0("nonabsdid puts treatment onset at relative time 0, ",
                       "so {.arg effects} post-period estimates correspond ",
                       "to {.code leads = effects - 1}. The underlying ",
                       "estimator call is identical."))
      }
    ))
  }

  list(values = values, dots = dots)
}
