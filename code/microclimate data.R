# Fetch the data from OSF PFTC6 ----
library(dataDownloader)

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
