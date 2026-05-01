#' @rdname as_nabs_event_study
#' @export
as_nabs_event_study.list <- function(x, method = NULL, outcome = NA_character_,
                                conf.level = 0.95, ...) {
  # Allow callers to dispatch on a list of native objects, returning a single
  # bound nabs_event_study_tbl. Useful for "give me everything in one tibble".
  parts <- lapply(x, as_nabs_event_study,
                  method = method, outcome = outcome,
                  conf.level = conf.level, ...)
  bind_event_studies(parts)
}
