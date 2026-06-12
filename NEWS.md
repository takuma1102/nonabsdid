# nonabsdid 0.3.2.9000

## Stata interoperability

* New `nabs_read_dta()`: read Stata `.dta` files into analysis-ready data
  frames. Value-labelled columns are converted to factors (or numeric codes
  with `labelled = "numeric"`) and Stata extended missing values (`.a`-`.z`)
  are collapsed to regular `NA` by default.
* New `nabs_write_dta()`: export an `nabs_event_study_tbl` (or anything
  coercible to one, including `nabs_event_study()` / `nabs_event_study_simple()`
  results) back to a `.dta` file, with schema columns renamed to Stata-valid
  variable names (`std.error` -> `std_error`, etc.).
* `nabs_event_study()` and `nabs_event_study_simple()` now accept a path to a
  `.dta` file as their `data` argument.
* Stata-style argument aliases are accepted by both wrappers and translated
  with an informative message: `df` (-> `data`), `group` (-> `unit`),
  `placebo` (-> `lags`), and `effects` (-> `leads = effects - 1`).
* New vignette: "nonabsdid for Stata users" -- an option-by-option mapping
  from Stata's `did_multiplegt_dyn` to `nabs_event_study()`, plus the full
  read-estimate-write-back-to-Stata round trip.
* `haven` added to Suggests.

# nonabsdid 0.3.2

* Initial CRAN release.
* Supports DCDH, PanelMatch, and fect (IFE/FE/MC) event-study estimators
  with a unified interface and tidy output schema.
* Provides `nabs_event_plot()` for overlaying estimates across methods,
  with an optional naive TWFE reference series.
