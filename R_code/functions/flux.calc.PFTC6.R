#load libraries

# library(tidyverse)
# library(lubridate)
# library(broom)

flux.calc.PFTC6 <- function(co2conc, # dataset of CO2 concentration versus time (output of match.flux)
                            chamber_volume = 24.5, # volume of the flux chamber in L, default for Three-D chamber (25x24.5x40cm)
                            tube_volume = 0.075, # volume of the tubing in L, default for summer 2020 setup
                            atm_pressure = 1, # atmoshperic pressure, assumed 1 atm
                            plot_area = 0.0625 # area of the plot in m^2, default for Three-D
)
{
  R = 0.082057 #gas constant, in L*atm*K^(-1)*mol^(-1)
  vol = chamber_volume + tube_volume
  slopes <- co2conc %>%
    group_by(fluxID) %>% #grouping CO2 concentration of each fluxes
    mutate(
      time = difftime(datetime[1:length(datetime)],datetime[1] , units = "secs") #the nb of seconds from the beginning of the measurement
    ) %>%
    select(fluxID, time, CO2) %>%
    do({model = lm(CO2 ~ time, data=.)    # create your model
    data.frame(tidy(model),              # get coefficient info
               glance(model))}) %>%          # get model info
    filter(term == "time") %>%
    rename(slope = estimate) %>%  #the slope is what we need to calculate the flux
    select(fluxID, slope, p.value, r.squared, adj.r.squared, nobs) %>%
    ungroup()

  means <- co2conc %>%
    group_by(fluxID) %>%
    summarise(
      PARavg = mean(PAR, na.rm = TRUE), #mean value of PAR for each flux
      temp_airavg = mean(temp_air, na.rm = TRUE)  #mean value of temp_air for each flux
      + 273.15, #transforming in kelvin for calculation
      temp_soilavg = mean(temp_soil, na.rm = TRUE) #mean value of temp_soil for each flux
    ) %>%
    ungroup()

  fluxes_final <- left_join(slopes, means, by = "fluxID") %>%
    left_join(
      co2conc,
      by = "fluxID"
    ) %>%
    select(fluxID, slope, adj.r.squared, p.value, PARavg, temp_airavg, temp_soilavg, turfID, type, start_window) %>%
    distinct() %>%
    rename(
      datetime = start_window
    ) %>%
    mutate(
      flux = (slope * atm_pressure * vol)/(R * temp_airavg * plot_area) #gives flux in micromol/s/m^2
      *3600 #secs to hours
      /1000 #micromol to mmol
      # PARavg = case_when(
      #   type == "ER" ~ NA_real_,
      #   type == "NEE" ~ PARavg
      # )
    ) %>% #flux is now in mmol/m^2/h, which is more common
    arrange(datetime) %>%
    select(!c(slope, temp_airavg))

  return(fluxes_final)

}
