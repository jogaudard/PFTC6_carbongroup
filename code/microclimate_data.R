#Soil moisture function ----
# NOTE: As of 2022.12.02 09:25 PST this script is ONLY for the PFTC6 C-flux microclimate data.
# We also need to add in the data from Three-D.

source("https://raw.githubusercontent.com/audhalbritter/Three-D/master/R/Climate/soilmoisture_correction.R")

# Fetch the data -----

library(dataDownloader)
library(tidyverse)
library(lubridate)

## from OSF PFTC6 ----
# Download microclimate data
get_file(node = "fcbw4",
         file = "PFTC6_microclimate_2022.zip",
         path = "raw_data",
         remote_path = "raw_data/c_flux_raw_data")

#Download microclimate metadata
get_file(node = "fcbw4",
         file = "PFTC6_microclimate_metadata_all.csv",
         path = "raw_data",
         remote_path = "raw_data/c_flux_raw_data")

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
metatomst <- read_csv("raw_data/PFTC6_microclimate_metadata.csv", col_types = "ffffffccccccccc") %>% #Note this file is American convention
  mutate(
    datetime_in = ymd_hm(datetime_in), #dates in correct format
    datetime_out = ymd_hms(datetime_out))  
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

### Three-D ----
#### Vik
# files3DVik <- dir(path = "raw_data/microclimate/climate/2022_autumn_Vik", pattern = "^data.*\\.csv$", full.names = TRUE, recursive = TRUE)
# 
# tempVik <- map_df(set_names(files3DVik), function(file) {
#   file %>% 
#     set_names() %>% 
#     map_df(~ read_csv2(file = file, col_names = FALSE)) #important! read_csv2 reads in European format
# }, .id = "File")
# 
# #### Joa
# files3DJoa <- dir(path = "raw_data/microclimate/climate/2022_autumn_Joa", pattern = "^data.*\\.csv$", full.names = TRUE, recursive = TRUE)
# 
# tempJoa <- map_df(set_names(files3DJoa), function(file) {
#   file %>% 
#     set_names() %>% 
#     map_df(~ read_csv2(file = file, col_names = FALSE)) #important! read_csv2 reads in European format
# }, .id = "File")
# 
# files3DLia <- dir(path = "raw_data/microclimate/climate/2022_autumn_Lia", pattern = "^data.*\\.csv$", full.names = TRUE, recursive = TRUE)
# 
# tempLia <- map_df(set_names(files3DLia), function(file) {
#   file %>% 
#     set_names() %>% 
#     map_df(~ read_csv2(file = file, col_names = FALSE)) #important! read_csv2 reads in European format
# }, .id = "File")
# 
# ### Grouped object
# tempThreeD = tempVik %>%
#   bind_rows(tempJoa) %>%
#   bind_rows(tempLia) %>%
#   # get logger ID 
#   mutate(
#     loggerID = str_sub(File, 52, 59), #adjust this for different file names
#     loggerID = as.factor(loggerID))
# 
# rm(tempVik)
# rm(tempJoa)
# rm(tempLia)




## make microclimate data ----
microclimate <- tempPFTC6 %>% 
  # bind_rows(tempThreeD) %>%
  # rename column names
  rename(ID = X1, datetime = X2, time_zone = X3, soil_temperature = X4, ground_temperature = X5, air_temperature = X6, RawSoilmoisture = X7, Shake = X8, ErrorFlag = X9) %>%
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
  # Filter to times the sensor was in the soil
  filter(datetime >= datetime_in & datetime <= datetime_out) %>%
  mutate(
    cutting = case_when(
      # air colder than expected
      sensor == "air_temperature" & value < -40 ~ "cut",
      # air warmer than expected
      sensor == "air_temperature" & value > 30 ~ "cut",
      # ground colder than expected
      sensor == "ground_temperature" & value < -40 ~ "cut",
      # ground warmer than expected
      sensor == "ground_temperature" & value > 35 ~ "cut",
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
      #Vikesland's time out seems to be wrong
      site == "Vikesland" & datetime > ymd_hm("2022-08-04 10:45") ~ "cut",
      TRUE ~ "keep"
    )
  )



# Graphs for visualizing cuts ----
## Air temperature ----
# microclimate.clean %>% filter(sensor == "air_temperature") %>% 
#   ggplot(aes(x = datetime, y = value, color = cutting)) +
#   geom_point(size = 0.04, aes(group = loggerID)) +
#   scale_color_manual(values = c(
#     "keep" = "#1e90ff",
#     "cut" = "#ff0800"
#   )) +
#   scale_x_datetime(date_breaks = "5 hour", date_labels = "%H:%M") +
#   # scale_x_date(date_labels = "%H:%M:%S") +
#   facet_wrap(vars(loggerID), scales = "free")
# 
# ggsave("air_temperature.png", height = 40, width = 100, units = "cm", path = "graph_microclimate")
# 
# ## Ground temperature ----
# ggplot(microclimate.clean %>% filter(sensor == "ground_temperature"), 
#        aes(x = datetime, y = value, color = cutting)) +
#   geom_point(size = 0.04, aes(group = loggerID)) +
#   scale_color_manual(values = c(
#     "keep" = "#1e90ff",
#     "cut" = "#ff0800"
#   )) +
#   scale_x_datetime(date_breaks = "5 hour", date_labels = "%H:%M") +
#   # scale_x_date(date_labels = "%H:%M:%S") +
#   facet_wrap(vars(loggerID), ncol = 3, scales = "free")
# 
# ## Soil temperature ----
# ggplot(microclimate.clean %>% filter(sensor == "soil_temperature"), 
#        aes(x = datetime, y = value, color = cutting)) +
#   geom_point(size = 0.04, aes(group = loggerID)) +
#   scale_color_manual(values = c(
#     "keep" = "#1e90ff",
#     "cut" = "#ff0800"
#   )) +
#   scale_x_datetime(date_breaks = "5 hour", date_labels = "%H:%M") +
#   # scale_x_date(date_labels = "%H:%M:%S") +
#   facet_wrap(vars(loggerID), ncol = 3, scales = "free")
# 
# ## Soil moisture ----
# ggplot(microclimate.clean %>% filter(sensor == "soil_moisture"), 
#        aes(x = datetime, y = value, color = cutting)) +
#   geom_point(size = 0.04, aes(group = loggerID)) +
#   scale_color_manual(values = c(
#     "keep" = "#1e90ff",
#     "cut" = "#ff0800"
#   )) +
#   scale_x_datetime(date_breaks = "5 hour", date_labels = "%H:%M") +
#   # scale_x_date(date_labels = "%H:%M:%S") +
#   facet_wrap(vars(loggerID), ncol = 3, scales = "free")


# Make clean CSV ----
microclimate.export <- microclimate.clean %>% 
  filter(
    cutting == "keep"
  ) %>% 
  select(datetime, loggerID, turfID, site, sensor, value) %>% 
  distinct()
  # group_by(datetime, loggerID, turfID, site, sensor, value) %>%
  # mutate(
  #   n = n()
  # ) %>% 
  # filter(n == 1) %>%  # we keep only the row that are unique
  # select(!n) %>% 
  # ungroup()

# taking Three-D data -----------------------------------------------------

threeD_microclimate_all <- read_csv("raw_data/microclimate/THREE-D_clean_microclimate_2019-2022.csv")

threeD_microclimate <- threeD_microclimate_all %>% 
  filter(
    # year(date_time) == 2022
    date_time >= ymd("2022-07-23")
    & date_time <= ymd("2022-08-05")
  ) %>%
  mutate(
    loggerID = as_factor(loggerID),
    # site = str_replace_all(destSiteID, c("Joa", "Vik", "Lia"), c("Joasete", "Vikesland", "Liahovden"))
    site = str_replace_all(destSiteID, c("Joa" = "Joasete", "Vik" = "Vikesland", "Lia" = "Liahovden"))
  ) %>% 
  rename(
    datetime = date_time,
    # site = destSiteID,
    datetime_in = initiale_date_time,
    datetime_out = end_date_time,
    soil_moisture = soilmoisture
  ) %>% 
  select(datetime, loggerID, turfID, site, soil_temperature, ground_temperature, air_temperature, soil_moisture, shake, error_flag, datetime_in, datetime_out) %>% 
  pivot_longer(cols = c(air_temperature, soil_temperature, ground_temperature, soil_moisture), names_to = "sensor", values_to = "value")

microclimate.export <- microclimate.export %>% 
  bind_rows(threeD_microclimate)


# more graphs to see the trends per site ----------------------------------

microclimate.export %>% 
  filter(
    sensor != "soil_moisture"
  ) %>% 
  ggplot(aes(datetime, value, color = site)) +
  geom_point(size = 0.2) +
  facet_grid(sensor~.)

microclimate.export %>% 
  filter(
    sensor == "soil_moisture"
  ) %>% 
  ggplot(aes(datetime, value, color = site)) +
  geom_point(size = 0.2)

write_csv(microclimate.export, "clean_data/PFTC6_microclimate_allsites_2022.csv")























