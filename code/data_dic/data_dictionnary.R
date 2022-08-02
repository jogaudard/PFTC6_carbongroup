#create data dictionnary for readme file
library(tidyverse)
library(lubridate)
library(dataDownloader)

# data import -------------------------------------------------------------
source("code/data_dic/download_read_clean_data.R")

# data dic function -------------------------------------------------------

# From Aud H Halbritter script
# https://github.com/audhalbritter/Three-D/blob/master/R/Rgathering/make_data_dic.R

make_data_dictionary <- function(data, description_table){
  
  # get range
  range <- data %>%
    as_tibble() %>%
    summarise(
      across(where(is.character), ~ paste(min(., na.rm = TRUE), max(., na.rm = TRUE), sep = " - ")),
      # across(where(is.character), ~ "-"),
      across(where(is.factor), ~ paste0(levels(.), collapse = ", ")),
      across(where(is.numeric), ~paste(round(min(., na.rm = TRUE), 3),round(max(., na.rm = TRUE), 3), sep = " - ")),
      across(where(is.Date), ~paste(min(., na.rm = TRUE), max(., na.rm = TRUE), sep = " - ")),
      across(where(is.POSIXct), ~paste(min(., na.rm = TRUE), max(., na.rm = TRUE), sep = " - "))
    ) %>%
    pivot_longer(cols = everything(), names_to = "Variable name", values_to = "Variable range or levels")
  
  # get class and make it into a tibble
  class <- map_dfr(data %>% as_tibble, ~enframe(class(.x)[1], value = "Variable type"),
                   .id = "Variable name") %>%
    select(-name) %>%
    # give sensible names
    mutate(`Variable type` = case_when(`Variable type` %in% c("character", "logical", "factor") ~ "categorical",
                                       `Variable type` %in% c("integer", "numeric") ~ "numeric",
                                       `Variable type` %in% c("Date") ~ "date",
                                       `Variable type` %in% c("POSIXct") ~ "date_time")) %>%
    # join with range
    left_join(range, by = "Variable name")
  
  # get class table with
  dictionary <- class %>%
     left_join(description_table, by = "Variable name") %>%
    select("Variable name", Description, "Variable type", "Variable range or levels", "Unit", "How measured")
  
 
    return(dictionary)
  }
  




# data description ------------------------------------------------------------
description <- read_csv("code/data_dic/data_dic.csv")

# metadata turf ------------------------------------------------------------------
# metadata_dic <- make_data_dictionary(data = metadata,
#                                   description_table = description
# )

# cflux ------------------------------------------------------------------
cflux_vikesland_dic <- make_data_dictionary(data = cflux_vikesland,
                                     description_table = description
)


# render readme --------------------------------------------------------
# to avoid re running everything and slowing down the process, we render the readme file here
# rmarkdown::render(input = "README.Rmd")
