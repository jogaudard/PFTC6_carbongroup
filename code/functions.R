
# Functions for the PFTC-6 carbon fluxes group

#load libraries

library(tidyverse)
library(lubridate)
library(broom)


# functions ----------------------------

# match.flux.PFTC6

match.flux.PFTC6 <- function(raw_flux, field_record, window_length = 90, startcrop = 10, measurement_length = 210, date_format = "dmy"){
  
  raw_flux <- raw_flux %>% 
    rename( #rename the columns with easier names to handle in the code
      datetime = "Date/Time",
      temp_air = "Temp_air ('C)",
      temp_soil = "Temp_soil ('C)",
      CO2 = "CO2 (ppm)",
      PAR = "PAR (umolsm2)"
    ) %>% 
    mutate(
      datetime = dmy_hms(datetime), #transform the date into R date format
      temp_air = as.numeric(temp_air),
      temp_soil = as.numeric(temp_soil),
      CO2 = as.numeric(CO2),
      PAR = as.numeric(PAR),
    ) %>% 
    select(datetime, temp_soil, temp_air, CO2, PAR)
  
  field_record <- field_record %>%
    mutate(
      starting_time = gsub("(\\d{2})(?=\\d{2})", "\\1:", starting_time, perl = TRUE), # to add the : in the time
      date = case_when(
        # !is.na(ymd(date)) ~ ymd(date),
        # !is.na(dmy(date)) ~ dmy(date)
        date_format == "ymd" ~ ymd(date),
        date_format == "dmy" ~ dmy(date),
        date_format == "mdy" ~ mdy(date)
      ),
      # date = dmy(date), #date in R format
      start = ymd_hms(paste(date, starting_time)), #pasting date and time together to make datetime
      end = start + measurement_length, #creating column End
      start_window = start + startcrop, #cropping the start
      end_window = start_window + window_length, #cropping the end of the measurement
      fluxID = row_number() #adding an individual ID ot each flux, useful to join data or graph the fluxes
    ) %>% 
    select(start, end, start_window, end_window, fluxID, turfID, type, date)
  
  
  co2conc <- full_join(raw_flux, field_record, by = c("datetime" = "start"), keep = TRUE) %>% #joining both dataset in one
    fill(PAR,temp_air, temp_soil, turfID,type,start,end,start_window, end_window, fluxID, date) %>% #filling all rows with data from above
    
    filter(
      datetime <= end
      & datetime >= start) %>% #cropping the part of the flux that is after the End and before the Start
    
    return(co2conc)
}



# flux.calc.PFTC6

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
    select(fluxID, slope, p.value, r.squared, adj.r.squared, nobs, PARavg, temp_airavg, temp_soilavg, turfID, type, start_window) %>% 
    distinct() %>% 
    rename(
      datetime = start_window
    ) %>% 
    mutate(
      flux = (slope * atm_pressure * vol)/(R * temp_airavg * plot_area) #gives flux in micromol/s/m^2
      *3600 #secs to hours
      /1000 #micromol to mmol
    ) %>% #flux is now in mmol/m^2/h, which is more common
    arrange(datetime) %>% 
    select(!slope)
  
  return(fluxes_final)
  
}



