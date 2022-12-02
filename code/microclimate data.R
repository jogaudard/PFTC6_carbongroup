#Soil moisture function ----

source("https://raw.githubusercontent.com/audhalbritter/Three-D/master/R/Climate/soilmoisture_correction.R")

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
## Read in metadata ----
# Read in meta data
metatomst <- read_csv("raw_data/PFTC6_microclimate_metadata.csv", col_types = "ffffffccccccccc") %>% #Note this file is American convention
  mutate(
    datetime_in = ymd_hm(datetime_in), #dates in correct format
    datetime_out = ymd_hms(datetime_out))  
## Read in files ----
# Make file list
files <- dir(path = "raw_data/microclimate", pattern = "^data.*\\.csv$", full.names = TRUE, recursive = TRUE)

# Read in data
temp <- map_df(set_names(files), function(file) {
    file %>% 
    set_names() %>% 
    map_df(~ read_csv2(file = file, col_names = FALSE)) #important! read_csv2 reads in European format
  }, .id = "File")

## make microclimate data ----
microclimate <- temp %>% 
  # rename column names
  rename(ID = X1, datetime = X2, time_zone = X3, soil_temperature = X4, ground_temperature = X5, air_temperature = X6, RawSoilmoisture = X7, Shake = X8, ErrorFlag = X9) %>%
  mutate(datetime = ymd_hm(datetime)
  ) %>% 
  # get logger ID 
  mutate(
    loggerID = str_sub(File, 28, 35), #adjust this for different file names
    loggerID = as.factor(loggerID)
  ) %>%
  select(!c(File, ID, X10)) %>% 
  distinct() %>%
  #join metdata
  left_join(metatomst, by = "loggerID") %>%
# calculate soil moisture
  # This function is written by Aud. Link at top of script.
  mutate( 
    soil_moisture = soil.moist(
      rawsoilmoist = RawSoilmoisture,
      soil_temp = soil_temperature,
      soilclass = "silt_loam" #it is the closest soil class, but still very wrong. The TMS calibration tool does not have any class for our soil
    )) %>%
  select(!c(RawSoilmoisture)) %>% # we want long tidy data
  pivot_longer(cols = c(air_temperature, soil_temperature, ground_temperature, soil_moisture), names_to = "sensor", values_to = "value")

# Clean data ----
microclimate.clean = microclimate %>%
  # Filter to times the sensor was in the soil
  filter(datetime > datetime_in & datetime < datetime_out) %>%
  mutate(
    cutting = case_when(
      # air colder than expected
      sensor == "air_temperature" & value < -40 ~ "cut",
      # air warmer than expected
      sensor == "air_temperature" & value > 20 ~ "cut",
      # soil colder than expected
      sensor == "soil_temperature" & value < 5 ~ "cut",
      # soil warmer than expected
      sensor == "soil_temperature" & value > 20 ~ "cut",
      # soil drier than expected
      sensor == "soil_moisture" & value < 0 ~ "cut",
      # soil wetter than expected
      sensor == "soil_moisture" & value > 0.5 ~ "cut",
      #Høgsete's time out seems to be wrong
      site == "Hogsete" & datetime > ymd_hm("2022-07-31 06:45") ~ "cut",
      TRUE ~ "keep"
    )
  )

# Graphs for visualizing cuts ----
  ggplot(microclimate.clean %>% filter(sensor == "soil_temperature"), 
         aes(x = datetime, y = value, color = cutting)) +
  geom_point(size = 0.04, aes(group = loggerID)) +
  scale_color_manual(values = c(
    "keep" = "#1e90ff",
    "cut" = "#ff0800"
  )) +
  scale_x_datetime(date_breaks = "5 hour", date_labels = "%H:%M") +
  # scale_x_date(date_labels = "%H:%M:%S") +
  facet_wrap(vars(loggerID), ncol = 3, scales = "free")
