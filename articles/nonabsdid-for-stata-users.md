# nonabsdid for Stata users

``` r

library(nonabsdid)
```

This vignette is for researchers whose main workflow is in Stata. It
covers:

1.  **Why bother**: which of the estimators wrapped here exist in Stata,
    and which are R-only.
2.  **Getting data in**: reading `.dta` files with
    [`nabs_read_dta()`](https://takuma1102.github.io/nonabsdid/reference/nabs_read_dta.md),
    and the labelled-variable / extended-missing pitfalls it handles for
    you.
3.  **A Rosetta stone**: option-by-option mapping from Stata’s
    `did_multiplegt_dyn` to
    [`nabs_event_study()`](https://takuma1102.github.io/nonabsdid/reference/nabs_event_study.md).
4.  **Stata-style argument aliases**: `group`, `effects`, `placebo`, and
    `df` are accepted directly.
5.  **Getting results out**: writing estimates back to `.dta` with
    [`nabs_write_dta()`](https://takuma1102.github.io/nonabsdid/reference/nabs_write_dta.md)
    so you (or a coauthor) can finish in Stata.

## 1. Why use R for this at all?

Of the heterogeneity-robust estimators that `nonabsdid` harmonizes, only
one has an official Stata implementation:

| Estimator | Stata | R |
|----|----|----|
| DCDH (de Chaisemartin & D’Haultfoeuille) | `did_multiplegt_dyn` (SSC) | `DIDmultiplegtDYN` |
| PanelMatch (Imai, Kim, & Wang) | — | `PanelMatch` |
| fect: IFE / FE-imputation / MC (Liu, Wang, & Xu) | — | `fect` |

If your treatment is **non-absorbing** (it can switch on and off) and
you want to compare DCDH against matching-based and
imputation/factor-model-based estimators on the same axis, R is
currently the only place where all of them live. `nonabsdid` exists to
make that comparison a few lines of code; this vignette exists to make
those lines feel familiar if you arrive from Stata.

Because the same DCDH estimator is implemented in both languages by the
same authors, the DCDH series is also your *bridge for trust*: run
`did_multiplegt_dyn` on the same data in Stata and through `nonabsdid`,
check that the point estimates agree, and then read the R-only
estimators with the same confidence. (Pin the version of
`DIDmultiplegtDYN` you used; see “Reproducibility” at the end.)

## 2. Getting your data in: `nabs_read_dta()`

The two classic stumbling blocks when moving a `.dta` file into R are:

- **Value labels.** Stata variables with `label values` arrive in R as
  `haven_labelled` vectors, which most estimation packages (including
  the ones wrapped here) do not understand.
- **Extended missing values.** Stata’s `.a`–`.z` arrive as *tagged*
  `NA`s, which look like ordinary `NA` when printed but are a distinct
  thing internally.

[`nabs_read_dta()`](https://takuma1102.github.io/nonabsdid/reference/nabs_read_dta.md)
handles both with sensible defaults: labelled columns become factors,
and all extended missings collapse to regular `NA`.

``` r

# For this vignette we fabricate a .dta file; in real life you already
# have one.
tmp <- tempfile(fileext = ".dta")
panel <- expand.grid(id = 1:60, t = 1:10)
panel$d <- with(panel, as.integer(
  (id %% 4 == 1 & t %in% 4:7) |
  (id %% 4 == 2 & t %in% 5:8) |
  (id %% 4 == 3 & t %in% 6:9)
))
panel$y <- 0.2 * panel$t + 0.5 * panel$d + rnorm(nrow(panel))
haven::write_dta(panel, tmp)

mydata <- nabs_read_dta(tmp)
#> Read /tmp/RtmpWda1kV/file26075054947d.dta: 600 rows, 4 columns.
head(mydata)
#> # A tibble: 6 × 4
#>      id     t     d      y
#>   <dbl> <dbl> <dbl>  <dbl>
#> 1     1     1     0 -1.20 
#> 2     2     1     0  0.455
#> 3     3     1     0 -2.24 
#> 4     4     1     0  0.194
#> 5     5     1     0  0.822
#> 6     6     1     0  1.35
```

If a labelled variable is really numeric — a 0/1 treatment dummy that
happens to carry “treated”/“untreated” labels is the common case — use
`labelled = "numeric"` to keep the underlying codes:

``` r

mydata <- nabs_read_dta("mypanel.dta", labelled = "numeric")
```

You can also skip the explicit read entirely:
[`nabs_event_study()`](https://takuma1102.github.io/nonabsdid/reference/nabs_event_study.md)
and
[`nabs_event_study_simple()`](https://takuma1102.github.io/nonabsdid/reference/nabs_event_study_simple.md)
accept a path to a `.dta` file as their `data` argument.

``` r

res <- nabs_event_study_simple(
  "mypanel.dta",
  outcome = "y", treatment = "d", unit = "id", time = "t"
)
```

## 3. Rosetta stone: `did_multiplegt_dyn` → `nabs_event_study()`

A typical Stata call:

``` stata
did_multiplegt_dyn y, group(id) time(t) treatment(d) ///
    effects(8) placebo(6) cluster(state) controls(x1 x2)
```

The equivalent through `nonabsdid`:

``` r

res <- nabs_event_study(
  mydata,
  outcome   = "y",
  treatment = "d",
  unit      = "id",     # Stata: group()
  time      = "t",
  method    = "DCDH",
  leads     = 7,        # Stata: effects(8)  -> leads = 8 - 1
  lags      = 6,        # Stata: placebo(6)
  cluster   = "state",
  controls  = c("x1", "x2")
)
```

Option by option:

| Stata (`did_multiplegt_dyn`) | [`nabs_event_study()`](https://takuma1102.github.io/nonabsdid/reference/nabs_event_study.md) | Note |
|----|----|----|
| `varlist` first variable (Y) | `outcome = "y"` |  |
| `group(id)` | `unit = "id"` |  |
| `time(t)` | `time = "t"` |  |
| `treatment(d)` | `treatment = "d"` |  |
| `effects(k)` | `leads = k - 1` | see below |
| `placebo(k)` | `lags = k` | same count of placebos |
| `cluster(v)` | `cluster = "v"` | defaults to `unit` |
| `controls(x1 x2)` | `controls = c("x1", "x2")` |  |
| any other option | pass through `...` | forwarded to [`DIDmultiplegtDYN::did_multiplegt_dyn()`](https://rdrr.io/pkg/DIDmultiplegtDYN/man/did_multiplegt_dyn.html) |

**Why `leads = effects - 1`?** Pure axis convention, not a difference in
the estimator. `did_multiplegt_dyn` counts `effects(k)` post-treatment
estimates labelled 1 through *k*; `nonabsdid` places treatment onset at
relative time 0, so a window of `leads` produces estimates at 0, 1, …,
`leads` — that is, `leads + 1` post-period estimates. `effects(8)` in
Stata and `leads = 7` here produce the *identical* underlying call and
the same number of estimated effects; only the x-axis labels shift by
one. The pre-period side has no shift: `placebo(6)` and `lags = 6` both
give six placebo estimates.

For options the unified wrapper doesn’t name explicitly
(e.g. `normalized`, `switchers`, `trends_nonparam`), pass them through
`...` using the R package’s argument names — they generally match the
Stata option names — or call
[`DIDmultiplegtDYN::did_multiplegt_dyn()`](https://rdrr.io/pkg/DIDmultiplegtDYN/man/did_multiplegt_dyn.html)
directly and tidy the result with
[`as_nabs_event_study()`](https://takuma1102.github.io/nonabsdid/reference/as_nabs_event_study.md).

### What about csdid / did_imputation / xtevent?

`csdid` (Callaway–Sant’Anna), `did_imputation`
(Borusyak–Jaravel–Spiess), and `eventstudyinteract` (Sun–Abraham) are
built for **absorbing** treatment (staggered adoption with no
reversals). If your treatment switches off, those designs don’t apply
directly — that is exactly the gap `nonabsdid`’s estimator set targets.
There is no option-level translation to give, because the estimators are
different; conceptually, your `csdid`-style event-study plot maps onto
[`nabs_event_study_simple()`](https://takuma1102.github.io/nonabsdid/reference/nabs_event_study_simple.md)’s
overlay figure.

## 4. Stata-style argument aliases

If you paste arguments from a Stata script, the wrappers understand the
Stata names directly and tell you how they were translated:

``` r

# These two calls are identical:
nabs_event_study(mydata, outcome = "y", treatment = "d", time = "t",
                 method = "DCDH",
                 group = "id", effects = 8, placebo = 6)
#> Translated Stata-style arguments:
#> * `group` -> `unit`
#> * `placebo` = 6 -> `lags` = 6
#> * `effects` = 8 -> `leads` = 7
#> i nonabsdid puts treatment onset at relative time 0, so `effects`
#>   post-period estimates correspond to `leads = effects - 1`. ...

nabs_event_study(mydata, outcome = "y", treatment = "d", time = "t",
                 method = "DCDH",
                 unit = "id", leads = 7, lags = 6)
```

`df` is likewise accepted for `data`. Supplying both a canonical name
and its alias (e.g. `unit` *and* `group`) is an error rather than a
silent choice.

## 5. Getting results out: `nabs_write_dta()`

Every estimator’s output lands in one tidy schema (`time`, `estimate`,
`std.error`, `conf.low`, `conf.high`, `window`, `method`, `outcome`), so
exporting all of it for a Stata-using coauthor is one line:

``` r

res <- nabs_event_study_simple(mydata, outcome = "y", treatment = "d",
                               unit = "id", time = "t")
nabs_write_dta(res$tidy, "event_study_results.dta")
```

Dots are not legal in Stata variable names, so `std.error`, `conf.low`,
and `conf.high` are renamed to `std_error`, `conf_low`, and `conf_high`
on the way out (you’ll see a message listing the renames).

Back in Stata, rebuilding the figure for one method is the usual
`twoway`:

``` stata
use event_study_results.dta, clear
keep if method == "DCDH"
twoway (rcap conf_low conf_high time) ///
       (scatter estimate time), ///
    yline(0, lpattern(dash)) xline(-0.5, lpattern(dot)) ///
    xtitle("Periods since treatment") ytitle("Effect on outcome") ///
    legend(off)
```

Or compare methods side by side:

``` stata
use event_study_results.dta, clear
encode method, gen(m)
twoway (scatter estimate time if m == 1) ///
       (scatter estimate time if m == 2) ///
       (scatter estimate time if m == 3), ///
    yline(0) legend(order(1 "DCDH" 2 "IFE" 3 "PanelMatch"))
```

[`nabs_write_dta()`](https://takuma1102.github.io/nonabsdid/reference/nabs_write_dta.md)
also accepts the result objects themselves (`nabs_event_study_result` /
`nabs_event_study_simple`) and routes them through
[`as_nabs_event_study()`](https://takuma1102.github.io/nonabsdid/reference/as_nabs_event_study.md)
for you.

## Reproducibility checklist

- **Cross-check DCDH.** Run `did_multiplegt_dyn` on the same data in
  both Stata and R once, and confirm the estimates match before relying
  on the R-only estimators.
- **Pin versions.** Record `packageVersion("DIDmultiplegtDYN")` (and the
  SSC version on the Stata side); the authors occasionally change
  defaults between releases.
- **Mind the axis.** When comparing figures across the two programs,
  remember the one-period shift in post-treatment labels described
  above.
