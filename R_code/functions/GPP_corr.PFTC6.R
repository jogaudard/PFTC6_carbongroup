#load libraries

# library(tidyverse)
# library(lubridate)
# library(broom)

# correcting for CO2 accumulated in canopy --------------------------------

GPP_corr.PFTC6 <- function(fluxes, start_night = "23:00:00", end_night = "04:00:00", strategy = c("mean", "max")){
  fluxes_corr <- fluxes %>%
    mutate(
      datetime = ymd_hms(datetime),
      time = as_hms(datetime),
      # time = hms(time),
      type = as_factor(type),
      difference = case_when(
        type == "GPP"
        & (time >= lubridate::hms(start_night) | time <= lubridate::hms(end_night))
        & flux > 0
        ~ flux
      ),
      corr_factor = case_when(
        strategy == "mean" ~ mean(difference, na.rm = TRUE),
        strategy == "max" ~ max(difference, na.rm = TRUE)
      ),
      flux_corrected = case_when(
        type == "ER" ~ flux,
        type == "GPP" ~ flux - corr_factor,
        type == "NEE" ~ flux - corr_factor
      )
    ) %>%
    select(!c(difference, corr_factor))

  return(fluxes_corr)
}
