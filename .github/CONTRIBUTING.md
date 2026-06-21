# Contributing to nonabsdid

Thanks for taking the time to contribute! This project welcomes issues and
pull requests. The notes below describe how to get involved.

## Code of Conduct

By participating in this project you agree to abide by its
[Code of Conduct](CODE_OF_CONDUCT.md). Please be respectful and constructive
in all interactions.

## How to contribute

### Reporting bugs and asking questions

- Search the [issue tracker](https://github.com/takuma1102/nonabsdid/issues)
  first to see whether your problem has already been reported.
- If not, open a new issue. For bugs, please include a minimal
  [reprex](https://reprex.tidyverse.org/) (reproducible example) and the
  output of `sessionInfo()` (or `sessioninfo::session_info()`).
- For estimator-specific behaviour, please note which backend you were using
  (`DIDmultiplegtDYN`, `PanelMatch`, or `fect`) and its version.

### Suggesting enhancements

Open an issue describing the use case and, where possible, a sketch of the
interface you have in mind. Because `nonabsdid` deliberately keeps a small,
unified surface over several estimators, please explain how the change fits
that goal.

### Pull requests

1. Fork the repository and create a branch from `main`.
2. Make your change. Keep the diff focused on a single concern.
3. Add or update tests under `tests/testthat/` so the new behaviour is
   covered. Tests that depend on a suggested estimator package should be
   guarded with `skip_if_not_installed()`.
4. Document any user-facing change with
   [roxygen2](https://roxygen2.r-lib.org/) and run `devtools::document()` to
   regenerate the `man/` files and `NAMESPACE`.
5. Run `R CMD check` (e.g. `devtools::check()`) and confirm it passes with no
   errors, warnings, or new notes.
6. Update `NEWS.md` if the change is user-facing.
7. Open a pull request referencing the issue it addresses. The PR template
   will prompt you for a short description of the change.

### Style

- This package uses roxygen2 for documentation and
  [testthat (edition 3)](https://testthat.r-lib.org/) for tests.
- Please follow the [tidyverse style guide](https://style.tidyverse.org/).
  You can check style locally with
  [`lintr`](https://lintr.r-lib.org/) and reformat with
  [`styler`](https://styler.r-lib.org/).
- Keep lines to 80 characters where practical.

## Development setup

```r
# install development dependencies
install.packages(c("devtools", "roxygen2", "testthat", "covr"))

# load the package for interactive work
devtools::load_all()

# run the tests
devtools::test()

# check test coverage
covr::report()
```

## Getting help

If you are unsure about anything, open an issue and ask. We are happy to help
you make your first contribution.

This project follows the
[rOpenSci contributing guidelines](https://devguide.ropensci.org/).
