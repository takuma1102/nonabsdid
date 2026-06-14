# nonabsdid 0.3.2.9001

Usability and large-panel robustness. No change to the output schema.

## New guard layer

* Added an internal `preflight_panel()` step, run at the entry of
  `nabs_event_study()` and `nabs_event_study_simple()`, that validates the
  panel and repairs the safe problems up front instead of surfacing cryptic
  upstream errors:
  - non-numeric `unit` / `cluster` columns are coerced to integer codes
    (PanelMatch requires a numeric unit id; DCDH's polars backend cannot
    cluster on a string). This only relabels ids and never changes estimates.
  - the treatment is checked to be a 0/1 (or `FALSE`/`TRUE`) indicator;
  - controls that are entirely `NA` raise a clear error;
  - partial missingness is reported, because estimators drop `NA` rows
    differently and the effective sample can differ across methods.
* DCDH now attaches `polars` automatically (with a one-time note), fixing the
  "object 'pl' not found" failure when polars is installed but not attached.

## New tuning knobs

* `nabs_event_study()` gained first-class arguments for the heavy estimators:
  `cv`, `nboots`, `r`, `parallel`, `cores` (the `fect` family) and
  `number.iterations` (PanelMatch's bootstrap). Passing `cv = FALSE` no longer
  collides with an internal default; conflicting values supplied through `...`
  are now dropped with a clear note instead of erroring.

## Lighter, safer defaults

* `fect` (`IFE`/`FE`/`MC`) now runs with `parallel = FALSE` by default. On
  large panels, copying the data to parallel workers exhausted memory
  (`future.globals.maxSize`) rather than helping; opt back in with
  `parallel = TRUE`.
* `nabs_event_study_simple()` is now a genuinely quick first pass:
  - default `methods` is `c("DCDH", "FE")`; the slower PanelMatch / IFE / MC
    are opt-in;
  - on panels with more than `max_units` (default 5000) units it uses a
    reproducible random sample of units unless `full = TRUE`;
  - `keep_fits = FALSE` by default, so the (potentially multi-GB) native
    estimator objects are not retained in `$fits`.
