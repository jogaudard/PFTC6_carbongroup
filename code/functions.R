
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
    mutate(
      type = as_factor(type),
      fluxID = as.numeric(fluxID)
    )
    
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


# GPP ---------------------------------------------------------------------

GPP.PFTC6 <- function(fluxes){
  
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
  select(!c(fluxID, adj.r.squared, p.value)) %>%
  # pivot_wider(names_from = type, values_from = PARavg, names_prefix = "PARavg_") %>% 
  # select(!c(PAR_corrected_flux)) %>%
  # select(campaign, turfID, date, type, corrected_flux) %>%
  pivot_wider(names_from = type, values_from = c(flux, temp_soilavg, datetime, PARavg)) %>% 
  
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
      type == "GPP" ~ rowMeans(select(., c(temp_soilavg_NEE, temp_soilavg_ER)), na.rm = TRUE)
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
    )
  ) %>% 
  select(!c(temp_soilavg_ER, temp_soilavg_NEE, PARavg_ER, PARavg_NEE, datetime_ER, datetime_NEE, pairID))

return(fluxes_GPP)

}


# correcting for CO2 accumulated in canopy --------------------------------

GPP_corr.PFTC6 <- function(fluxes, start_night = "23:00:00", end_night = "04:00:00", strategy = c("mean", "max")){
  fluxes_corr <- fluxes %>% 
    mutate(
      datetime = ymd_hms(datetime),
      time = hms::as_hms(datetime),
      # time = hms(time),
      type = as_factor(type),
      difference = case_when(
        type == "GPP" 
        & (time >= hms(start_night) | time <= hms(end_night))
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


# zhao2018 ----------------------------------------------------------------

# function to select window automatically and calculate slope based on zhao et al 2018

# function providing the df with the two fits, linear and non linear
fitting.flux <- function(data,
                         weird_fluxesID = NA, # a vector of fluxes to discard because they are obviously wrong
                         t_window = 20,
                         Cz_window = 15,
                         b_window = 10){
  
  data <- co2_fluxes_vikesland %>% 
    filter(
      fluxID %in% c(100:115)
    )
  
  CO2_df <- data %>% 
    group_by(fluxID) %>% 
    mutate(
      time = difftime(datetime[1:length(datetime)],datetime[1] , units = "secs"),
      time = as.double(time),
      tmax = max(datetime[CO2 == max(CO2)]),
      tmin = min(datetime[CO2 == min(CO2)]),
      start_cut = case_when(
        tmax > tmin ~ tmin - t_window,
        tmin > tmax ~ tmax - t_window
      ),
      end_cut = case_when(
        tmax < tmin ~ tmin + t_window,
        tmin < tmax ~ tmax + t_window
      ),
      start_window = case_when(
        start_cut > start_window ~ start_cut,
        TRUE ~ start_window
      ),
      end_window = case_when(
        end_cut < end_window ~ end_cut,
        TRUE ~ end_window
      ),
      cut = case_when(
        datetime <= start_window | datetime >= end_window ~ "cut",
        # fluxID ==  & datetime %in%  ~ "cut",
        fluxID %in% weird_fluxesID ~ "cut",
        TRUE ~ "keep"
      ),
      cut = as_factor(cut)
    ) %>% 
    ungroup()
  
  cut_CO2_df <- CO2_df %>% 
    group_by(fluxID) %>% 
    filter(cut == "keep") %>% 
    mutate(
      time_cut = difftime(datetime[1:length(datetime)],datetime[1] , units = "secs"),
      time_cut = as.double(time_cut)
    ) %>% 
      ungroup()
  
  Cm_df <- cut_CO2_df %>% 
    group_by(fluxID) %>% 
    distinct(CO2, .keep_all = TRUE) %>% 
    # mutate(
    #   time = difftime(datetime[1:length(datetime)],datetime[1] , units = "secs"),
    #   time = as.double(time)
    # ) %>% 
    mutate(
      Cmax = max(CO2),
      Cmin = min(CO2),
      tmax = time_cut[CO2 == Cmax],
      tmin = time_cut[CO2 == Cmin]
    ) %>% 
    select(fluxID, Cmax, Cmin, tmax, tmin) %>% 
    ungroup() %>% 
    distinct(Cmax, Cmin, .keep_all = TRUE)
  
  Cm_slope <- cut_CO2_df %>% 
    group_by(fluxID) %>% 
    # mutate(
    #   time = difftime(datetime[1:length(datetime)],datetime[1] , units = "secs"),
    #   time = as.double(time)
    # ) %>% 
    do({model = lm(CO2 ~ time_cut, data=.)    # create your model
    data.frame(tidy(model),              # get coefficient info
               glance(model))}) %>%          # get model info
    filter(term == "time_cut") %>% 
    rename(slope_Cm = estimate) %>% 
    select(fluxID, slope_Cm) %>% 
    ungroup()
  
  Cm_df <- left_join(Cm_df, Cm_slope) %>% 
    mutate(
      Cm_est = case_when(
        slope_Cm < 0 ~ Cmin, 
        slope_Cm > 0 ~ Cmax 
      ),
      tm = case_when(
        slope_Cm < 0 ~ tmin,
        slope_Cm > 0 ~ tmax
      )
    ) %>% 
    select(fluxID, Cm_est, tm, slope_Cm) %>% 
    ungroup()
  
  Cz_df <- cut_CO2_df %>%
    group_by(fluxID) %>%
    # mutate(
    #   time = difftime(datetime[1:length(datetime)],datetime[1] , units = "secs"),
    #   time = as.double(time)
    # ) %>%
    # select(fluxID, time, CO2) %>%
    filter(
      time_cut <= Cz_window
    ) %>%
    do({model = lm(CO2 ~ time_cut, data=.)    # create your model
    data.frame(tidy(model),              # get coefficient info
               glance(model))}) %>%          # get model info
    pivot_wider(id_cols = fluxID, names_from = "term", values_from = "estimate") %>% 
    rename(
      Cz = "(Intercept)",
      slope_Cz = time_cut) %>%
    select(fluxID, Cz, slope_Cz) %>%
    ungroup()
  
  tz_df <- cut_CO2_df %>% 
    # left_join(Cm_df) %>% 
    left_join(Cz_df) %>% 
    group_by(fluxID) %>% 
    # mutate(
    #   time = difftime(datetime[1:length(datetime)],datetime[1] , units = "secs"),
    #   time = as.numeric(time)
    # ) %>% 
    filter(
      time_cut > Cz_window
    ) %>% 
    mutate(
      Cd = abs(CO2-Cz),
      tz_est = min(time_cut[Cd == min(Cd)])
    ) %>% 
    ungroup() %>% 
    select(fluxID, tz_est) %>% 
    distinct()
  
  a_df <- cut_CO2_df %>% 
    group_by(fluxID) %>% 
    filter(
      datetime >= end_window - t_window
    ) %>% 
    do({model = lm(CO2 ~ time_cut, data=.)    # create your model
    data.frame(tidy(model),              # get coefficient info
               glance(model))}) %>%          # get model info
    pivot_wider(id_cols = fluxID, names_from = "term", values_from = "estimate") %>% 
    rename(
      a_est = time_cut
      ) %>% 
    select(fluxID, a_est) %>% 
    ungroup()
  
  Ct_df <- cut_CO2_df %>% 
    left_join(tz_df) %>% 
    group_by(fluxID) %>% 
    mutate(
      Ct = CO2[time_cut == tz_est - Cz_window]
    ) %>% 
    ungroup() %>% 
    select(fluxID, Ct) %>% 
    distinct()
  
  estimates_df <- left_join(Cm_df, Cz_df) %>% 
    left_join(tz_df) %>% 
    left_join(a_df) %>% 
    left_join(Ct_df) %>% 
    mutate(
      b_est = log((Ct - Cm_est + a_est * Cz_window)/(Cz - Cm_est)) * (1/Cz_window)
    )
  
  myfn <- function(data, par) {
    with(data, sqrt((1/length(time)) * sum((par[1]+par[2]*(time-par[4])+(Cz-par[1])*exp(-par[3]*(time-par[4]))-CO2)^2)))
  }
  
  myoptim <- function(Cm_est, a_est, b_est, tz_est, data) {
    results <- optim(par = c(Cm_est, a_est, b_est, tz_est), fn = myfn, data = data)
    return(results$par)
  }
  
  # problem: need to loop the optim on each group of fluxID with specific par per group. MAybe I need to do a for loop
  # other idea: make a tibble with a column with par as vector and CO2 and time in another column as a df, and summarize optim on that
  
  fitting_par <- cut_CO2_df %>% 
    left_join(estimates_df) %>% 
    group_by(fluxID) %>%
    # filter(fluxID == 111) %>%
    summarize(
      # Cm = map(., optim(par = c(estimates_df$Cm_est[fluxID], estimates_df$a_est[fluxID], estimates_df$b_est[fluxID], estimates_df$tz_est[fluxID]), fn = myfn, data = .))$par[1],
      # a = apply(., c(1,2,3,4), optim(par = c(Cm_est, a_est, b_est, tz_est), fn = myfn, data = .))$par[2],
      # b = mean(CO2)
      # b = apply(myoptim(Cm_est, a_est, b_est, tz_est, data = CO2))
      ID = fluxID,
      b = optim(par = c(estimates_df$Cm_est[estimates_df$fluxID==ID], estimates_df$a_est[estimates_df$fluxID==ID], estimates_df$b_est[estimates_df$fluxID==ID], estimates_df$tz_est[estimates_df$fluxID==ID]), fn = myfn, data = .)$par[3]
      # tz = optim(par = c(.$Cm_est, .$a_est, .$b_est, .$tz_est), fn = myfn, data = .)$par[4],
      # slope_tz = a + b * (Cm - Cz)
    ) %>%
    ungroup() %>% 
    distinct()
    filter(fluxID, Cm, a, b, tz, slope_tz, Cz)
  
  CO2_fitting <- CO2_df %>% 
    left_join(fitting_par) %>% 
    mutate(
      time_corr = difftime(start_window[1],datetime[1] , units = "secs"), # need a correction because in this df time is starting at beginning, not at cut
      time_corr = as.double(time_corr),
      fit = Cm + a * (time- tz - time_corr) + (Cz - Cm) * exp(- b * (time- tz - time_corr)),
      fit_slope = slope_tz * (time - time_corr) + Cz - slope_tz * tz
    )
    
  return(CO2_fitting)
}
  
  
  
# function to graph specific fluxID or RMSE above a certain threshold
  
# function calculating fluxes for each fluxID and providiing RMSE
