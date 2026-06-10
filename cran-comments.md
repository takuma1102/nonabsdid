## R CMD check results

## Test environments
- local Windows install, R 4.5.1
- R-hub: linux, windows, ubuntu-release, donttest (R-devel)
- win-builder (devel and release)

0 errors | 0 warnings | 1 note

## Resubmission

This is a resubmission. I got valuable comments from Ms. Konstanze Lauseker on the 10th.
In this version I have:

* Removed single quotes around function names in the Description field.
* Added references for the implemented methods to the Description field in
  the requested authors (year) <doi:...> format.
* Removed \dontrun{} from examples and all quick, self-contained examples are now
  unwrapped. Examples requiring optional estimator packages are guarded
  appropriately. Also, as one estimator uses 'polars' package, which is not included
  in CRAN, I added necessary information about the package in the Description field.
* Replaced installed.packages() in tests with system.file(package = ...),
  avoiding namespace loading for optional estimator packages.
* Corrected the naive TWFE benchmark specification (in naive_TWFE.R). The previous
  setting was   statistically inappropriate for the intended non-absorbing-treatment
  event-study comparison.
* Aligned the DCDH reference period with the reference period used by the
  other supported estimators , so that harmonized event-time estimates are
  comparable across methods.

The last two changes were found during the resubmission checks and they are not
direct responses to your review comments, but they affect the statistical
correctness and comparability of the package output. I apologize for including
these additional changes unrelated to your previous comments at resubmission, 
and would be grateful if they could also be reviewed as part of this resubmission.

R CMD check --as-cran was run locally on the submitted tarball.
There were no ERRORs or WARNINGs.

## Downstream dependencies
There are currently no downstream dependencies for this package.