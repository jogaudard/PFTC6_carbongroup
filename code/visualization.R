my_packages <- c("dataDownloader",
                 "tidyverse",
                 "lubridate"
)

lapply(my_packages, library, character.only = TRUE) 

get_file(node = "fcbw4",
         file = "PFTC6_24h_cflux_allsites_2022.csv",
         path = "clean_data",
         remote_path = "c_flux_data")