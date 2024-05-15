#load libraries

# library(tidyverse)
# library(lubridate)
# library(broom)
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
      # ) %>%
      # arrange(start) %>%
      # mutate(
      fluxID = row_number() #adding an individual ID to each flux, useful to join data or graph the fluxes
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

