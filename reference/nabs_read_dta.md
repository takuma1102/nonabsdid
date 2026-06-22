# Read a Stata .dta file into an analysis-ready data frame

\`nabs_read_dta()\` is a thin convenience layer over
\[haven::read_dta()\] that smooths out the two places where freshly
imported Stata data tends to trip up R estimation packages:

## Usage

``` r
nabs_read_dta(
  path,
  labelled = c("factor", "numeric", "keep"),
  missings = c("na", "keep"),
  encoding = NULL,
  verbose = TRUE,
  ...
)
```

## Arguments

- path:

  Path to a \`.dta\` file.

- labelled:

  How to handle \`haven_labelled\` columns. One of:

  \`"factor"\` (default)

  :   Convert labelled columns to factors via \[haven::as_factor()\].
      Unlabelled values keep their code as the level name.

  \`"numeric"\`

  :   Strip value labels via \[haven::zap_labels()\], keeping the
      underlying numeric codes. Use this when a labelled column is
      really a numeric variable (e.g. a 0/1 treatment dummy that happens
      to carry labels).

  \`"keep"\`

  :   Leave \`haven_labelled\` columns untouched. Note that the
      estimator packages may not accept them.

- missings:

  How to handle Stata extended missing values (\`.a\`–\`.z\`). \`"na"\`
  (default) collapses them to regular \`NA\` via
  \[haven::zap_missing()\]; \`"keep"\` preserves the tags.

- encoding:

  Passed to \[haven::read_dta()\]. Only needed for files written by
  Stata 13 or older with a non-default encoding.

- verbose:

  Logical; if \`TRUE\` (default), print a one-line summary of what was
  read and converted.

- ...:

  Additional arguments passed to \[haven::read_dta()\] (e.g.
  \`col_select\`, \`n_max\`).

## Value

A tibble.

## Details

- \*\*Labelled columns.\*\* Stata value labels arrive in R as
  \`haven_labelled\` vectors, which many modeling functions (including
  the estimator packages wrapped by nonabsdid) do not understand. By
  default these are converted to factors; set \`labelled = "numeric"\`
  to drop the labels and keep the underlying codes instead.

- \*\*Extended missing values.\*\* Stata's \`.a\`–\`.z\` arrive as
  \*tagged\* \`NA\`s, which compare and print like ordinary \`NA\` but
  can survive into model matrices in surprising ways. By default all
  tagged \`NA\`s are collapsed to regular \`NA\`.

Variable labels (Stata's \`label variable\`) are preserved as
\`"label"\` attributes on each column; they are harmless to the
estimators and often useful for plot labels.

You rarely need to call this function yourself: \[nabs_event_study()\]
and \[nabs_event_study_simple()\] accept a path to a \`.dta\` file as
their \`data\` argument and route it through \`nabs_read_dta()\`
automatically.

## See also

\[nabs_write_dta()\] for the reverse direction, and the "nonabsdid for
Stata users" vignette (\`vignette("nonabsdid-for-stata-users")\`) for a
full Stata-to-R walk-through.

## Examples

``` r
if (requireNamespace("haven", quietly = TRUE)) {
  # Round-trip a small labelled panel through a temporary .dta file.
  tmp <- tempfile(fileext = ".dta")
  panel <- data.frame(id = rep(1:3, each = 2), t = rep(1:2, 3),
                      d = c(0, 1, 0, 0, 1, 1),
                      y = rnorm(6))
  haven::write_dta(panel, tmp)

  mydata <- nabs_read_dta(tmp)
  head(mydata)
}
#> Read /tmp/Rtmp002gH2/file1ec4c4c5951.dta: 6 rows, 4 columns.
#> # A tibble: 6 × 4
#>      id     t     d      y
#>   <dbl> <dbl> <dbl>  <dbl>
#> 1     1     1     0 -0.341
#> 2     1     2     1  1.50 
#> 3     2     1     0  0.528
#> 4     2     2     0  0.542
#> 5     3     1     1 -0.137
#> 6     3     2     1 -1.14 
```
