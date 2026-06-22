# Toy data generation and visualization for nonabsdid
# ============================================================
# Builds a synthetic panel with a non-absorbing binary treatment and
# heterogeneous dynamic effects, then renders the nonabsdid event-study plots.
#
# Run from a fresh R session, top to bottom.
# Note: `pl` is the polars namespace handle, so never use it as a variable name.

# --- Clear leftover objects --------------------------------------------------
# A stale `pl` would mask polars' pl$col() and break the DCDH backend.
if (exists("pl")) rm(pl)
rm(list = ls())

# --- First-time setup (normally left commented out) --------------------------
# pak::pak("takuma1102/nonabsdid")
# Sys.setenv(NOT_CRAN = "true")
# remotes::install_github("pola-rs/r-polars@v1.9.0")   # pin polars at 1.9.0
# install.packages("S7")
# install.packages("DIDmultiplegtDYN")
# install.packages(c("fect", "PanelMatch", "fixest"))

library(polars)
library(DIDmultiplegtDYN)
library(nonabsdid)

set.seed(42)

## 1. Panel skeleton ----------------------------------------------------------
N <- 200   # units
T <- 20    # periods

## 2. Non-absorbing treatment path d ------------------------------------------
# Persistent Markov process: OFF->ON with prob p_on, ON persists with p_stay.
gen_d <- function(T, p_on = 0.15, p_stay = 0.75) {
  d <- integer(T)
  state <- 0L
  for (tt in seq_len(T)) {
    state <- if (state == 0L) rbinom(1, 1, p_on) else rbinom(1, 1, p_stay)
    d[tt] <- state
  }
  d
}

d_mat <- t(replicate(N, gen_d(T, p_on = 0.08, p_stay = 0.92)))  # N x T matrix

## 3. Fixed effects and heterogeneity -----------------------------------------
alpha <- rnorm(N, 0, 1.0)            # unit fixed effects
gamma <- cumsum(rnorm(T, 0, 0.3))    # time fixed effects (mild trend)

# Effect heterogeneity theta_i, correlated with treatment frequency
# (more-treated units have larger effects), which biases TWFE.
treat_freq <- rowMeans(d_mat)
theta <- 1 + 1.5 * scale(treat_freq)[, 1] + rnorm(N, 0, 0.3)

## 4. Distributed-lag dynamic effect ------------------------------------------
# y_it = alpha_i + gamma_t + theta_i*(0.5 d_it + 1.0 d_{i,t-1} + 1.5 d_{i,t-2}) + e
beta <- c(0.5, 1.0, 1.5)             # lag 0,1,2 effects (cumulative -> dynamic)
lagm <- function(m, k) {
  cbind(matrix(0, nrow(m), k), m[, seq_len(ncol(m) - k), drop = FALSE])
}

effect <- theta * (beta[1] * d_mat +
                   beta[2] * lagm(d_mat, 1) +
                   beta[3] * lagm(d_mat, 2))

## 5. Messy panel error e -----------------------------------------------------
# Widen the CIs toward realistic panel noise via (a) a sizable error SD,
# (b) within-unit AR(1) serial correlation, and (c) unit-level heteroskedasticity.
sigma <- 3.0
rho   <- 0.5
eps <- matrix(0, N, T)
eps[, 1] <- rnorm(N, 0, sigma)
for (tt in 2:T) {
  eps[, tt] <- rho * eps[, tt - 1] + rnorm(N, 0, sigma * sqrt(1 - rho^2))
}
unit_scale <- runif(N, 0.7, 1.6)     # per-unit dispersion
eps <- eps * unit_scale              # scale by row (unit)

## 6. Assemble the outcome y --------------------------------------------------
y_mat <- outer(alpha, rep(1, T)) +
         outer(rep(1, N), gamma) +
         effect +
         eps

## 7. Long-format data frame --------------------------------------------------
mydata <- data.frame(
  id = rep(seq_len(N), times = T),
  t  = rep(seq_len(T), each = N),
  y  = as.vector(y_mat),
  d  = as.vector(d_mat)
)
mydata <- mydata[order(mydata$id, mydata$t), ]

head(mydata)
table(mydata$d)                       # both ON and OFF present
sum(tapply(mydata$d, mydata$id, function(x) any(diff(x) != 0)))  # switching units

## 8. Run each estimator over a common window ---------------------------------
res_dcdh <- nabs_event_study(mydata, outcome = "y", treatment = "d",
                             unit = "id", time = "t", method = "DCDH",
                             lags = 4, leads = 4)

res_ife  <- nabs_event_study(mydata, outcome = "y", treatment = "d",
                             unit = "id", time = "t", method = "IFE",
                             lags = 4, leads = 4)

# TWFE reference
ref <- naive_twfe(mydata, outcome = "y", treatment = "d",
                  unit = "id", time = "t", lags = 4, leads = 4)

## 9. PanelMatch (called directly) --------------------------------------------
# Do not bind the result to `pl` (it collides with polars' pl).
library(PanelMatch)

pd <- PanelData(panel.data = mydata, unit.id = "id", time.id = "t",
                treatment = "d", outcome = "y")

pm <- PanelMatch(panel.data = pd,
                 lag = 4, lead = 0:4,
                 refinement.method = "none",         # no covariates -> none
                 qoi = "att",
                 forbid.treatment.reversal = FALSE,   # non-absorbing -> FALSE
                 match.missing = TRUE,
                 placebo.test = TRUE)

pe <- PanelEstimate(sets = pm, panel.data = pd, se.method = "bootstrap")
pm_placebo <- placebo_test(pm.obj = pm, panel.data = pd,
                           plot = FALSE, se.method = "bootstrap")

tidy_pm <- as_nabs_event_study(pe, pre_obj = pm_placebo,
                               method = "PanelMatch", outcome = "y")

## 10. Overlay all four methods and save for the README (two styles) ----------

## --- Style A (default): colour by method x pre/post -------------------------
p_color <- nabs_event_plot(
  res_dcdh$tidy, res_ife$tidy, tidy_pm,
  reference = ref,
  xlim = c(-4, 4),
  ylim = c(-2, 6),
  ylab = "Effect on y",
  dodge = 0.6
)

## --- Style B: colour by method only, pre/post by marker shape ---------------
p_shape <- nabs_event_plot(
  res_dcdh$tidy, res_ife$tidy, tidy_pm,
  reference = ref,
  style = "method_shape",
  xlim = c(-4, 4),
  ylim = c(-2, 6),
  ylab = "Effect on y",
  dodge = 0.6
)

p_shape_connect <- nabs_event_plot(
  res_dcdh$tidy, res_ife$tidy, tidy_pm,
  reference = ref,
  style = "method_shape",
  xlim = c(-4, 4),
  ylim = c(-2, 6),
  ylab = "Effect on y",
  dodge = 0.6,
  connect = TRUE
)

p_color   # preview (default)
p_shape   # preview (shape style)
p_shape_connect

## To join point estimates with a thin line, add connect = TRUE:
#   nabs_event_plot(res_dcdh$tidy, res_ife$tidy, tidy_pm, reference = ref,
#                   connect = TRUE, dodge = 0.6)                         # A + line
#   nabs_event_plot(res_dcdh$tidy, res_ife$tidy, tidy_pm, reference = ref,
#                   style = "method_shape", connect = TRUE, dodge = 0.6) # B + line

dir.create("man/figures", recursive = TRUE, showWarnings = FALSE)

ggplot2::ggsave(
  "man/figures/README_example_plot.png",                # style A (default)
  plot  = p_color,
  width = 7, height = 4, dpi = 150,
  bg    = "white"
)
ggplot2::ggsave(
  "man/figures/README_example_plot_method_shape.png",   # style B
  plot  = p_shape,
  width = 7, height = 4, dpi = 150,
  bg    = "white"
)

ggplot2::ggsave(
  "man/figures/README_example_plot_method_shape_connect.png",   # style B + line
  plot  = p_shape_connect,
  width = 7, height = 4, dpi = 150,
  bg    = "white"
)
