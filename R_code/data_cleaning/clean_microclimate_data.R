#Soil moisture function ----

source("https://raw.githubusercontent.com/audhalbritter/Three-D/master/R/functions/soilmoisture_correction.R")

# metadata

source("R_code/data_cleaning/make_metadata.R")

# Fetch the data -----

library(dataDownloader)
library(tidyverse)
library(lubridate)

## from OSF PFTC6 ----
# Download microclimate data
get_file(node = "fcbw4",
         file = "PFTC6_microclimate_2022.zip",
         path = "raw_data",
         remote_path = "raw_data/vii. microclimate_raw_data")

#Download microclimate metadata
get_file(node = "fcbw4",
         file = "PFTC6_microclimate_metadata_all.csv",
         path = "raw_data",
         remote_path = "raw_data/vii. microclimate_raw_data")


#Unzip microclimate data
unzip("raw_data/PFTC6_microclimate_2022.zip", exdir = "raw_data/microclimate")
file.remove("raw_data/PFTC6_microclimate_2022.zip") #let's free some space

## from OSF Three-D ----
get_file(node = "pk4bg",
         file = "THREE-D_clean_microclimate_2019-2022.csv.zip",
         path = "raw_data",
         remote_path = "Climate")

#Unzip microclimate data
unzip("raw_data/THREE-D_clean_microclimate_2019-2022.csv.zip", exdir = "raw_data/microclimate")
file.remove("raw_data/THREE-D_clean_microclimate_2019-2022.csv.zip") #let's free some space

# Climate data ----
## Read in metadata ----
# Read in meta data
metatomst <- read_csv("raw_data/PFTC6_microclimate_metadata_all.csv", col_types = "ffffffccccccccc") %>% #Note this file is American convention
  mutate(
    datetime_in = mdy_hm(datetime_in), #dates in correct format
    datetime_out = mdy_hm(datetime_out))

# modifying metadata to have a dataset that covers as much as possible of the course (23.07.2022 to 07.08.2022), but not more
course_start <- ymd_hms("2022-07-23T00:00:01")
course_end <- ymd_hms("2022-08-08T00:00:01")

metatomst <- metatomst %>%
  mutate(
    datetime_in = case_when(
      datetime_in <= course_start ~ course_start,
      datetime_in > course_start ~ datetime_in
    ),
    datetime_out = case_when(
      datetime_out >= course_end ~ course_end,
      datetime_out < course_end ~ datetime_out
    )
  ) |>
  rename(destSiteID = site)

## Read in files ----
### PFTC6 ----
# Make file list
filesPFTC6 <- dir(path = "raw_data/microclimate", pattern = "^data.*\\.csv$", full.names = TRUE, recursive = TRUE)

# Read in data
tempPFTC6 <- map_df(set_names(filesPFTC6), function(file) {
  file %>%
    set_names() %>%
    map_df(~ read_csv2(file = file, col_names = FALSE, col_types = "dcdddddddl")) #important! read_csv2 reads in European format
}, .id = "File")%>%
  # get logger ID
  mutate(
    loggerID = str_sub(File, 28, 35), #adjust this for different file names
    loggerID = as.factor(loggerID))

# make microclimate data ----
microclimate <- tempPFTC6 %>%
  # bind_rows(tempThreeD) %>%
  # rename column names
  rename(ID = X1, datetime = X2, time_zone = X3, soil_temperature = X4, ground_temperature = X5, air_temperature = X6,
         RawSoilmoisture = X7, Shake = X8, ErrorFlag = X9) %>%
  mutate(datetime = ymd_hm(datetime)
  ) %>%
  select(!c(File, ID, X10)) %>%
  distinct() %>%
  #join metdata
  right_join(metatomst, by = "loggerID") %>%  #Right join to filter out irrelevant loggers
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
  # This is now either the full days of the flux observations (midnight to midnight)
  # or when the sensor was in the soil
  # whichever is longer
  filter(datetime > datetime_in & datetime < datetime_out) %>%
  mutate(
    flag = case_when(
      # air colder than expected
      sensor == "air_temperature" & value < -40 ~ "cut_Tmin_air",
      # air warmer than expected
      sensor == "air_temperature" & value > 30 ~ "cut_Tmax_air",
      # ground colder than expected
      sensor == "ground_temperature" & value < -40 ~ "cut_Tmin_ground",
      # ground warmer than expected
      sensor == "ground_temperature" & value > 35 ~ "cut_Tmax_ground",
      # soil colder than expected
      sensor == "soil_temperature" & value < 5 ~ "cut_Tmin_soil",
      # soil warmer than expected
      sensor == "soil_temperature" & value > 20 ~ "cut_Tmax_soil",
      # soil drier than expected
      sensor == "soil_moisture" & value < 0 ~ "cut_min_moist",
      # # soil wetter than expected # no such thing in wester Norway
      # sensor == "soil_moisture" & value > 0.5 ~ "cut_max_moist"
      TRUE ~ "keep"
    )) |>
  # Replace flagged values with NA
  rename(climate_variable = sensor) |>
  mutate(value = case_when(
    flag != "keep" ~ NA,
    TRUE ~ value
  )) |>
  select(datetime, loggerID, turfID, destSiteID, climate_variable,
         value, flag, datetime_in, datetime_out)

# Adding Three-D data -----------------------------------------------------
threeD_microclimate_all <- read_csv("raw_data/microclimate/THREE-D_clean_microclimate_2019-2022.csv")

threeD_microclimate <- threeD_microclimate_all %>%
  filter(
    # year(date_time) == 2022
    date_time >= course_start
    & date_time <= course_end
  ) %>%
  mutate(
    loggerID = as_factor(loggerID),
    destSiteID = str_replace_all(destSiteID, c("Joa" = "Joasete", "Vik" = "Vikesland", "Lia" = "Liahovden"))
  ) %>%
  rename(
    datetime = date_time,
    datetime_in = initiale_date_time,
    datetime_out = end_date_time,
    soil_moisture = soilmoisture
  ) %>%
  select(datetime, loggerID, turfID, destSiteID, soil_temperature, ground_temperature, air_temperature, soil_moisture, datetime_in, datetime_out) %>%
  pivot_longer(cols = c(air_temperature, soil_temperature, ground_temperature, soil_moisture), names_to = "sensor", values_to = "value") %>%
  mutate(
    flag = case_when( # even tho the Three-D data were cleaned by the Three-D team, we apply the same filter that was applied to our data, out of consistency
      # air colder than expected
      sensor == "air_temperature" & value < -40 ~ "cut_Tmin_air",
      # air warmer than expected
      sensor == "air_temperature" & value > 30 ~ "cut_Tmax_air",
      # ground colder than expected
      sensor == "ground_temperature" & value < -40 ~ "cut_Tmin_ground",
      # ground warmer than expected
      sensor == "ground_temperature" & value > 35 ~ "cut_Tmax_ground",
      # soil colder than expected
      sensor == "soil_temperature" & value < 5 ~ "cut_Tmin_soil",
      # soil warmer than expected
      sensor == "soil_temperature" & value > 20 ~ "cut_Tmax_soil",
      # soil drier than expected
      sensor == "soil_moisture" & value < 0 ~ "cut_min_moist",
      # soil wetter than expected #no such thing in Western Norway
      # sensor == "soil_moisture" & value > 0.5 ~ "cut_max_moist"
      TRUE ~ "keep"
    )) |>
  # # Replace flagged values with NA
  rename(climate_variable = sensor) |>
  mutate(value = case_when(
    flag != "keep" ~ NA,
    TRUE ~ value
  )) |>
  select(datetime, loggerID, turfID, destSiteID, datetime_in, datetime_out,
         climate_variable, value, flag)

microclimate.export <- microclimate.clean %>%
  bind_rows(threeD_microclimate) %>%
  left_join(metaturf) |>
  relocate(datetime, loggerID, turfID, origSiteID, destSiteID, turfID, loggerID, warming, datetime_in, datetime_out,
           climate_variable, value, flag)

write_csv(microclimate.export, "clean_data/PFTC6_clean_GlobalChangeExperiment_microclimate_2022.csv")

# to write number of flags in data paper
microclimate.export %>% select(flag, value) %>% count(flag)
