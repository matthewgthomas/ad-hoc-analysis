# Chan and Kawalerowicz (2024) show that social diversity (based on four measures)
# is not statistically associated with social cohesion (based on six measures)
# when accounting for material deprivation.
#
# But the paper only presents regression tables. How do various levels of diversity
# and deprivation actually affect predicted social cohesion?
#
# This script simulates predicted values of Buckner's index (one of the six cohesion
# measures) across a based on the percentage of foreign-born people in a neighbourhood
# (one of the four diversity measures) and the Townsend index (a measure of material
# deprivation).
#
# Buckner's neighbourhood cohesion index is measured on a 1-5 scale, where
# 5 = very strong cohesion (i.e. better relationship with neighbours and stronger
# attachment to the neighbourhood).
#
# The Townsend index is a z-score where higher values indicate more deprivation.
#
# FINDINGS:
# - Higher %s of foreign-born people do not predict lower social cohesion.
# - Material deprivation (higher Townsend) predicts slightly lower social cohesion.
#
# Sources:
# - Chan, T. W., & Kawalerowicz, J. (2024). Social diversity and social cohesion in Britain.
#.  https://doi.org/10.1111/1468-4446.13094
# - Norman, P. (2016). The changing geography of deprivation in Britain: 1971 to 2011 and beyond.
#   https://www.researchgate.net/publication/306283801_The_Changing_Geography_of_Deprivation_in_Britain_1971_to_2011_and_Beyond

library(ggplot2)
library(dplyr)
library(readr)
library(nomisr)

# --- Model coefficients -------------------------------------------------------
# These coefficients are based on model 2 in regression table A9.
# Coefficients
b0_hat   <- 3.027   # constant
b_fb_hat <- -0.073  # % foreign-born (as share 0..1)
b_tz_hat <- -0.022  # Townsend (z-score; SD ≈ 3.5)

# Standard errors
se_b0   <- 0.035
se_b_fb <- 0.081
se_b_tz <- 0.002

#' Simulate expected Buckner's index over a grid of (% foreign-born, townsend)
#'
#' @param pct_foreign_seq numeric vector of percentages (0..100). Will be converted to share (0..1).
#' @param townsend_seq numeric vector of Townsend scores (z-ish; SD about 3.5)
#' @param n_sims number of coefficient draws
#' @param clip logical; clamp predictions to [1,5]
#' @return data.frame with mean, median, and 90%/95% intervals for each grid point
simulate_buckner <- function(pct_foreign_seq = seq(0, 100, by = 5),
                             townsend_seq    = seq(-7, 7, by = 0.25),
                             n_sims = 5000,
                             clip = TRUE) {

  # Draw coefficients from their sampling distributions (assume independence)
  b0_draw   <- rnorm(n_sims, b0_hat,   se_b0)
  b_fb_draw <- rnorm(n_sims, b_fb_hat, se_b_fb)
  b_tz_draw <- rnorm(n_sims, b_tz_hat, se_b_tz)

  # Build grid
  grid <- expand.grid(
    pct_foreign = pct_foreign_seq,
    townsend    = townsend_seq
  )
  grid$share_foreign <- grid$pct_foreign / 100

  # For each grid point, compute simulated expected values
  pred_summ <- lapply(seq_len(nrow(grid)), function(i) {
    mu <- b0_draw +
      b_fb_draw * grid$share_foreign[i] +
      b_tz_draw * grid$townsend[i]

    if (clip) mu <- pmin(5, pmax(1, mu))

    c(mean = mean(mu),
      median = median(mu),
      p05 = quantile(mu, 0.05),
      p10 = quantile(mu, 0.10),
      p25 = quantile(mu, 0.25),
      p75 = quantile(mu, 0.75),
      p90 = quantile(mu, 0.90),
      p95 = quantile(mu, 0.95))
  })
  pred_summ <- do.call(rbind, pred_summ)
  out <- cbind(grid[, c("pct_foreign","townsend")], as.data.frame(pred_summ))
  rownames(out) <- NULL
  out
}

# ---- Get realistic range of % foreign-born -----------------------------------
# Get data on country of birth by MSOA (2021) from Nomis
# nomis_get_metadata(id = "NM_2024_1", "geography", "TYPE")
# nomis_get_metadata(id = "NM_2024_1", "c2021_cob_12")
# nomis_get_metadata(id = "NM_2024_1", "measures")

cob_raw <-
  nomis_get_data(
    id = "NM_2024_1",
    date = "latest",
    geography = "TYPE152", # MSOA (2021)
    c2021_cob_12 = "1",  # Born in the UK
    measures = "20301" # Percent
  )

range(100 - cob_raw$OBS_VALUE)
#--> Between 1.5% and 72% of people in MSOAs were not born in the UK

# --- Predict social cohesion --------------------------------------------------
# Simulate across a realistic range of foreign-born and Townsend from -7 to +7
pred_grid <- simulate_buckner(
  # pct_foreign_seq = seq(0, 100, by = 2),
  pct_foreign_seq = seq(1.5, 72, by = 2),  # realistic range based on 2021 data
  townsend_seq    = seq(-7, 7, by = 0.25),
  n_sims = 8000
)

# Heatmap of the expected (mean) Buckner's index
ggplot(pred_grid, aes(pct_foreign, townsend, fill = mean)) +
  geom_raster(interpolate = TRUE) +
  labs(x = "% foreign-born",
       y = "Townsend (higher = more deprived)",
       fill = "Buckner (mean)",
       title = "Simulated Buckner's index from Model 2",
       subtitle = "Partial effect of % foreign-born and Townsend; others at baseline (intercept)") +
  coord_fixed(ratio = 100/14) +
  theme_minimal()

# Slice plots (effect of foreign-born at a few Townsend levels)
slice_levels <- c(-3.5, 0, 3.5, 7)  # ~ -1 SD, mean, +1 SD, +2 SD (given SD ≈ 3.5)
pred_slices <- pred_grid %>%
  filter(townsend %in% slice_levels)

ggplot(pred_slices, aes(pct_foreign, mean, group = factor(townsend))) +
  geom_line() +
  geom_ribbon(aes(ymin = `p05.5%`, ymax = `p95.95%`), alpha = 0.2) +
  facet_wrap(~ townsend, nrow = 1,
             labeller = label_bquote(Townsend == .(townsend))) +
  scale_y_continuous(breaks = seq(1, 5, by = 1), limits = c(1, 5)) +
  labs(x = "% foreign-born", y = "Buckner's index (expected)",
       title = "Effect of % foreign-born at fixed Townsend levels",
       subtitle = "Ribbon = 90% simulation interval (coefficient uncertainty)") +
  theme_minimal()
