#create data dictionary for readme file ----
library(tidyverse)
library(lubridate)
library(dataDownloader)

# data import -------------------------------------------------------------
source("code/data_dic/download_read_clean_data.R")

# Code import
source("code/functions/make_data_dictionary.R")

# data description ------------------------------------------------------------
description_cflux <- read_csv("code/data_dic/data_dic_cflux.csv")
description_microclimate <- read_csv("code/data_dic/data_dic_microclimate.csv")

# metadata turf ------------------------------------------------------------------
# metadata_dic <- make_data_dictionary(data = metadata,
#                                   description_table = description
# )

# cflux ------------------------------------------------------------------
# cflux <- bind_rows(cflux_vikesland, cflux_hogsete, cflux_liahovden)

cflux_dic <- make_data_dictionary(data = cflux,
                                  description_table = description_cflux
)

# Microclimate ----
microclimate_dic <- make_data_dictionary(data = microclimate,
                                         description_table = description_microclimate)

# render readme --------------------------------------------------------
# to avoid re running everything and slowing down the process, we render the readme file here
rmarkdown::render(input = "README.Rmd")
