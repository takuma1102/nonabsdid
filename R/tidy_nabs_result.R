#' @rdname as_nabs_event_study
#' @export
as_nabs_event_study.nabs_event_study_result <- function(
  x,
  method = NULL,
  outcome = NA_character_,
  conf.level = 0.95,
  ...
) {
  as_nabs_event_study(
    x$tidy,
    method = method,
    outcome = outcome,
    conf.level = conf.level,
    ...
  )
}

#' @rdname as_nabs_event_study
#' @export
as_nabs_event_study.nabs_event_study_simple <- function(
  x,
  method = NULL,
  outcome = NA_character_,
  conf.level = 0.95,
  ...
) {
  as_nabs_event_study(
    x$tidy,
    method = method,
    outcome = outcome,
    conf.level = conf.level,
    ...
  )
}
