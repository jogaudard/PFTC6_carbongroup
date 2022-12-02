# Fetch the data from OSF PFTC6 ----
library(dataDownloader)
library(tidyverse)
library(lubridate)

# Download microclimate data
get_file(node = "fcbw4",
         file = "PFTC6_microclimate_2022.zip",
         path = "raw_data",
         remote_path = "raw_data/c_flux_raw_data")

#Download microclimate metadata
get_file(node = "fcbw4",
         file = "PFTC6_microclimate_metadata.csv",
         path = "raw_data",
         remote_path = "raw_data/c_flux_raw_data")

#Unzip microclimate data
unzip("raw_data/PFTC6_microclimate_2022.zip", exdir = "raw_data/microclimate")
file.remove("raw_data/PFTC6_microclimate_2022.zip") #let's free some space

# Climate data ----
## Read in files ----
# Make file list
files <- dir(path = "raw_data/microclimate", pattern = "^data.*\\.csv$", full.names = TRUE, recursive = TRUE)

# Read in data
temp <- map_df(set_names(files), function(file) {
    file %>% 
    set_names() %>% 
    map_df(~ read_delim(file = file, col_names = FALSE, delim = ";"))
  }, .id = "File")

## clean data ----
microclimate <- temp %>% 
  # rename column names
  rename("ID" = "X1", "datetime" = "X2", "time_zone" = "X3", "soil_temperature" = "X4", "ground_temperature" = "X5", "air_temperature" = "X6", "RawSoilmoisture" = "X7", "Shake" = "X8", "ErrorFlag" = "X9") %>%
  mutate(
    datetime = as.character(datetime),
    datetime = substr(datetime, start = 0, stop = 16), #some dates are in ymd_hms format
    datetime = ymd_hm(datetime)
  ) %>% 
  # Soil moisture calibration
  #mutate(SoilMoisture = a * RawSoilmoisture^2 + b * RawSoilmoisture + c) %>% 
  # get logger ID -> not needed anymore, have whole filename now!!!
  mutate(
    loggerID = substr(File, nchar(File)-13, nchar(File)-6),
    loggerID = as.factor(loggerID)
  ) %>% 
  select(!c(File, ID)) %>% 
  distinct() 
  left_join(metatomst, by = "loggerID") %>% 
  # group_by(loggerID) %>%
  # mutate(
  #   date_out = replace_na(date_out, today("CET")) #the logger still in the field don't have a date_out (NA in the metaData), but we need a date_out to filter. Today's date can only be correct because if you are never running this script on future data ;-)
  # ) %>% 
  filter(
    datetime > date_in + 1
    & datetime < date_out #maybe we don't have the data from the logger that broke, which is why are getting an empty df with that line
  ) %>% 
  mutate( # calculate soil moisture
    soil_moisture = soil.moist(
      rawsoilmoist = RawSoilmoisture,
      soil_temp = soil_temperature,
      soilclass = "silt_loam" #it is the closest soil class, but still very wrong. The TMS calibration tool does not have any class for our soil
    )) %>% 
  select(!c(RawSoilmoisture, logger)) %>% # we want vertical tidy data
  pivot_longer(cols = c(air_temperature, soil_temperature, ground_temperature, soil_moisture), names_to = "sensor", values_to = "value")