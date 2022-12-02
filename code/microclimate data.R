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

#Unzip microclimate data
unzip("raw_data/PFTC6_microclimate_2022.zip", exdir = "raw_data/microclimate")
file.remove("raw_data/PFTC6_microclimate_2022.zip") #let's free some space
