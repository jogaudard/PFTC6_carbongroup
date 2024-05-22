#load libraries

# library(tidyverse)
# library(lubridate)
# library(broom)

GPP.PFTC6 <- function(fluxes){

  #just to test
  # fluxes <- cflux_liahovden

  fluxes_GPP <- fluxes %>%
    mutate(
      pairID = case_when(
        type == "NEE" ~ fluxID,
        type == "ER" ~ fluxID-1
      ),   # problem with datetime, it is different between ER and NEE. Let's use datetime NEE
      # datetime = case_when(
      #   type == "NEE" ~ datetime,
      #   type == "ER" ~ NA_real_
      # ),
      turfID = as_factor(turfID),
      type = as_factor(type)
    ) %>%
    select(pairID, PARavg, temp_soilavg, turfID, type, datetime, flux, fluxID, flag, flux_noflag, temp_airavg) %>%
    # select(!c(fluxID, adj.r.squared, p.value)) %>%
    # select(!c(fluxID)) %>%
    # pivot_wider(names_from = type, values_from = PARavg, names_prefix = "PARavg_") %>%
    # select(!c(PAR_corrected_flux)) %>%
    # select(campaign, turfID, date, type, corrected_flux) %>%
    pivot_wider(names_from = type, values_from = c(flux, temp_soilavg, datetime, PARavg, fluxID, flag, flux_noflag, temp_airavg)) %>%

    # pivot_wider(names_from = type, values_from = c(flux, temp_soilavg)) %>%
    rename(
      ER = flux_ER,
      NEE = flux_NEE
    ) %>%
    mutate(
      GPP = NEE - ER
    ) %>%
    pivot_longer(c(ER, NEE, GPP), names_to = "type", values_to = "flux") %>%
    mutate(
      temp_soil = case_when(
        type == "ER" ~ temp_soilavg_ER,
        type == "NEE" ~ temp_soilavg_NEE,
        # type == "GPP" ~ rowMeans(select(., c(temp_soilavg_NEE, temp_soilavg_ER)), na.rm = TRUE)
        type == "GPP" ~ NA_real_ # I think it should be NA as it was not measured. The average is not really telling anything.
      ),
      temp_airavg = case_when(
        type == "ER" ~ temp_airavg_ER,
        type == "NEE" ~ temp_airavg_NEE,
        # type == "GPP" ~ rowMeans(select(., c(temp_airavg_NEE, temp_airavg_ER)), na.rm = TRUE)
        type == "GPP" ~ NA_real_
      ),
      PARavg = case_when(
        type == "ER" ~ PARavg_ER,
        type == "NEE" ~ PARavg_NEE,
        type == "GPP" ~ PARavg_NEE
      ),
      datetime = case_when(
        type == "ER" ~ datetime_ER,
        type == "NEE" ~ datetime_NEE,
        type == "GPP" ~ datetime_NEE
      ),
      fluxID = case_when(
        type == "ER" ~ fluxID_ER,
        type == "NEE" ~ fluxID_NEE,
        type == "GPP" ~ NA_real_
      ),
      flag = case_when(
        type == "ER" ~ flag_ER,
        type == "NEE" ~ flag_NEE,
        type == "GPP" & flag_ER == "discard" ~ "discard",
        type == "GPP" & flag_NEE == "discard" ~ "discard",
        type == "GPP" & flag_ER == "zero" ~ "zeroER",
        type == "GPP" & flag_NEE == "zero" ~ "zeroNEE",
        type == "GPP" & flag_ER == "start_error" ~ "start_error",
        type == "GPP" & flag_NEE == "start_error" ~ "start_error",
        type == "GPP" & flag_ER == "weird_flux" ~ "weird_flux",
        type == "GPP" & flag_NEE == "weird_flux" ~ "weird_flux",
        type == "GPP" & flag_ER == "ok" & flag_NEE == "ok" ~ "ok",
      ),
      flux_noflag = case_when(
        type == "ER" ~ flux_noflag_ER,
        type == "NEE" ~ flux_noflag_NEE,
        type == "GPP" ~ NA_real_
      )
    ) %>%
    select(!c(
      temp_soilavg_ER,
      temp_soilavg_NEE,
      PARavg_ER,
      PARavg_NEE,
      datetime_ER,
      datetime_NEE,
      pairID,
      fluxID_NEE,
      fluxID_ER,
      flag_NEE,
      flag_ER,
      flux_noflag_NEE,
      flux_noflag_ER,
      temp_airavg_NEE,
      temp_airavg_ER
    ))

  return(fluxes_GPP)

}
