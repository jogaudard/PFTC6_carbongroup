library(dataDownloader)
# library(tidyverse)

# All CFlux data ----
get_file(node = "fcbw4",
         file = "PFTC6_24h_cflux_allsites_2022.csv",
         path = "clean_data",
         remote_path = "v. c_flux_data")

cflux <- read_csv("clean_data/PFTC6_24h_cflux_allsites_2022.csv")

# Microclimate data ----
get_file(node = "fcbw4",
         file = "PFTC6_clean_microclimate_2022.csv",
         path = "clean_data",
         remote_path = "vii. microclimate_data")

microclimate <- read_csv("clean_data/PFTC6_clean_microclimate_2022.csv") |>
  mutate(loggerID = as.numeric(loggerID))
