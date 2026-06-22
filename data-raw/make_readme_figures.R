# data-raw/make_readme_figures.R
#
# Regenerates the cohort-matrix figure shown in README.md. Run from the package
# root after installing fect (and, if you want the DCDH panel, DIDmultiplegtDYN
# + polars). This script is excluded from the build (data-raw/ is in
# .Rbuildignore); the PNG it writes lives in man/figures/ and IS shipped.
#
#   Rscript -e 'devtools::load_all()' -e 'source("data-raw/make_readme_figures.R")'

devtools::load_all(".")

set.seed(1)
N <- 120; TT <- 14
panel <- expand.grid(id = 1:N, t = 1:TT)
grp   <- panel$id %% 4
onset <- c(`1` = 4L, `2` = 6L, `3` = 8L)[as.character(grp)]
panel$onset <- onset
off <- (panel$id %% 8 == 1) & !is.na(panel$onset) & panel$t >= panel$onset + 3L
panel$d <- as.integer(!is.na(panel$onset) & panel$t >= panel$onset & !off)
ui  <- rnorm(N, sd = 0.5)[panel$id]
tau <- ifelse(panel$d == 1, 0.4 + 0.05 * (panel$t - panel$onset), 0)
panel$y <- ui + 0.15 * panel$t + tau + rnorm(nrow(panel))
panel$onset <- NULL

stopifnot(requireNamespace("fect", quietly = TRUE))
res <- nabs_effect_cells(
  panel, outcome = "y", treatment = "d", unit = "id", time = "t",
  method = "IFE", lags = 4, leads = 6, nboots = 200
)

p <- plot_effect_matrix(res$cells, show_estimates = TRUE, show_se = TRUE)

# bg = "white" is belt-and-suspenders; the theme already paints a white canvas.
ggplot2::ggsave(
  "man/figures/README_cohort_matrix.png", p,
  width = 8, height = 4.2, dpi = 200, bg = "white"
)
message("wrote man/figures/README_cohort_matrix.png")
