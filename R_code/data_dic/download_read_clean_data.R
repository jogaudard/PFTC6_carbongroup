library(dataDownloader)
# library(tidyverse)

# All CFlux data ----
get_file(node = "fcbw4",
         file = "PFTC6_clean_GlobalChangeExperiment_cflux_2022.csv",
         path = "clean_data",
         remote_path = "v. c_flux_data")

cflux <- read_csv("clean_data/PFTC6_clean_GlobalChangeExperiment_cflux_2022.csv")

# Microclimate data ----
get_file(node = "fcbw4",
         file = "PFTC6_clean_GlobalChangeExperiment_microclimate_2022.csv",
         path = "clean_data",
         remote_path = "vii. microclimate")

microclimate <- read_csv("clean_data/PFTC6_clean_GlobalChangeExperiment_microclimate_2022.csv") |>
  mutate(loggerID = as.numeric(loggerID))
