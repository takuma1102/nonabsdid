# Write event-study results to a Stata .dta file

\`nabs_write_dta()\` exports an \`nabs_event_study_tbl\` – or anything
that \[as_nabs_event_study()\] can coerce into one, including the result
objects returned by \[nabs_event_study()\] and
\[nabs_event_study_simple()\] – to a Stata \`.dta\` file via
\[haven::write_dta()\].

## Usage

``` r
nabs_write_dta(x, path, version = 14, label = NULL, verbose = TRUE)
```

## Arguments

- x:

  An \`nabs_event_study_tbl\`, an \`nabs_event_study_result\`, an
  \`nabs_event_study_simple\`, a supported estimator object, or a plain
  data frame with at least \`time\` and \`estimate\` columns. Anything
  that is not already a data frame is routed through
  \[as_nabs_event_study()\].

- path:

  Path of the \`.dta\` file to write.

- version:

  Stata file format version, passed to \[haven::write_dta()\]. Default
  \`14\` (readable by Stata 14 and later).

- label:

  Optional dataset label (Stata's \`label data\`), passed to
  \[haven::write_dta()\].

- verbose:

  Logical; if \`TRUE\` (default), print a one-line summary including any
  column renames.

## Value

The path, invisibly.

## Details

The tidy schema uses dots in some column names (\`std.error\`,
\`conf.low\`, \`conf.high\`), which are not valid Stata variable names.
These are renamed to underscore versions (\`std_error\`, \`conf_low\`,
\`conf_high\`) on the way out; any other invalid characters are likewise
replaced with \`\_\`.

This makes the "estimate in R, post-process in Stata" workflow a
one-liner: a Stata-using coauthor can rebuild the event-study figure
with \`twoway rcap\`/\`scatter\`, or feed the estimates into their own
tables.

## See also

\[nabs_read_dta()\] for the reverse direction.

## Examples

``` r
if (requireNamespace("haven", quietly = TRUE)) {
  tidy <- as_nabs_event_study(
    data.frame(time = -2:3,
               estimate = c(0.02, -0.01, 0, 0.4, 0.5, 0.45),
               std.error = 0.1),
    method = "DCDH", outcome = "y"
  )
  tmp <- tempfile(fileext = ".dta")
  nabs_write_dta(tidy, tmp)

  haven::read_dta(tmp)
}
#> Wrote 6 rows to /tmp/Rtmpx6hoTg/file1ecd541294b8.dta (Stata version 14).
#> ℹ Renamed for Stata: std.error -> std_error, conf.low -> conf_low, conf.high ->
#>   conf_high.
#> # A tibble: 6 × 8
#>    time estimate std_error conf_low conf_high window method outcome
#>   <dbl>    <dbl>     <dbl>    <dbl>     <dbl> <chr>  <chr>  <chr>  
#> 1    -2     0.02       0.1   -0.176     0.216 pre    DCDH   y      
#> 2    -1    -0.01       0.1   -0.206     0.186 pre    DCDH   y      
#> 3     0     0          0.1   -0.196     0.196 post   DCDH   y      
#> 4     1     0.4        0.1    0.204     0.596 post   DCDH   y      
#> 5     2     0.5        0.1    0.304     0.696 post   DCDH   y      
#> 6     3     0.45       0.1    0.254     0.646 post   DCDH   y      
```
