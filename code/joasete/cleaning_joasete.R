
# This script will be to separate c-flux data into turfIDs and clean the fluxes before we calculate them

source("code/functions.R")
library("dataDownloader")
library(tidyverse)

# download raw data
# download files from OSF ---------------------------------------

get_file(node = "pk4bg",
         file = "Three-D_24h-cflux_joasete_2022.csv",
         path = "raw_data",
         remote_path = "RawData/C-Flux")

get_file(node = "pk4bg",
         file = "Three-D_24h-cflux_joasete_2_2022.csv",
         path = "raw_data",
         remote_path = "RawData/C-Flux")

get_file(node = "pk4bg",
         file = "PFTC6_cflux_field-record_joasete.csv",
         path = "raw_data",
         remote_path = "RawData/C-Flux")

get_file(node = "pk4bg",
         file = "PFTC6_cflux_cutting_joasete.csv",
         path = "raw_data",
         remote_path = "RawData/C-Flux")

# If you manage to download dataDownloader and download the data, you are good! Congrats!
# In case you did not manage to download the data manually. Call me :-)

# cleaning Vikesland ------------------------------------------------------
# read the files
# Joasete was done in two times because of a licor failure
co2_24h_joasete <- read_csv("raw_data/Three-D_24h-cflux_joasete_2022.csv", na = c("#N/A"), col_types = "cfdddddd") %>% 
  bind_rows(
    read_csv("raw_data/Three-D_24h-cflux_joasete_2_2022.csv", na = c("#N/A"), col_types = "cfdddddd")
  )
  
record_joasete <- read_csv("raw_data/PFTC6_cflux_field-record_joasete.csv", na = c(""))




# matching the CO2 concentration data with the turfs using the field record
# we have defined a default window length of 90 secs.

co2_fluxes_joasete_90 <- match.flux.PFTC6(co2_24h_joasete, record_joasete, window_length = 90, date_format = "ymd")
# at Joasete, turfID 29 WN3C 106 and 109 AN3C 109 were swapped between 2022-07-28 11:20:00 and 2022-07-29 00:30:00

co2_fluxes_joasete_90 <- co2_fluxes_joasete_90 %>% 
  mutate(
    turfID_correct = case_when(
      datetime %in% c(ymd_hms("2022-07-28T11:20:00"):ymd_hms("2022-07-29T00:30:00"))
      & turfID == "29 WN3C 106"
      ~ "109 AN3C 109",
      datetime %in% c(ymd_hms("2022-07-28T11:20:00"):ymd_hms("2022-07-29T00:30:00"))
      & turfID == "109 AN3C 109"
      ~ "29 WN3C 106",
      TRUE ~ turfID
    )
  ) %>% 
  select(!turfID) %>% 
  rename(
    turfID = "turfID_correct"
  )
# cutting Vikesland ------------------------------------------------------
cutting_joasete <- read_csv("raw_data/PFTC6_cflux_cutting_joasete.csv", na = "", col_types = "dtt")

co2_cut_joasete_90 <- co2_fluxes_joasete_90 %>% 
  left_join(cutting_joasete, by = "fluxID") %>% 
  mutate(
    start_cut = ymd_hms(paste(date, .$start_cut)),
    end_cut = ymd_hms(paste(date, .$end_cut))
  )

# adjusting the time window with manual cuts ------------------------------------------------------

co2_cut_joasete_90 <- co2_cut_joasete_90 %>%
  mutate(
  start_window = case_when(
    is.na(start_cut) == FALSE ~ start_cut,
    TRUE ~ start_window
  ),
  end_window = case_when(
    is.na(end_cut) == FALSE ~ end_cut,
    TRUE ~ end_window
  ),
  cut = case_when(
    datetime <= start_window | datetime >= end_window ~ "cut",
    # fluxID ==  & datetime %in%  ~ "cut",
    TRUE ~ "keep"
    ),
  cut = as_factor(cut)
  )


# vizz Vikesland -------------------------------------------------------

# visualizing 90 secs cuts in Vikesland (it´s in comments, just in case you don´t want to visualize it)

# theme_set(theme_grey(base_size = 5)) 
# 
# co2_cut_vikesland_90 %>% 
#   ggplot(aes(x = datetime, y = CO2, colour = cut)) +
#   geom_line(size = 0.2, aes(group = fluxID)) +
#   # geom_line(size = 0.2) +
#   scale_x_datetime(date_breaks = "1 min", minor_breaks = "10 sec", date_labels = "%e/%m \n %H:%M") +
#   # scale_x_date(date_labels = "%H:%M:%S") +
#   facet_wrap(vars(fluxID), ncol = 30, scales = "free")
#   
# ggsave("fluxes_details_vikesland.png", height = 40, width = 80, units = "cm")


# produce clean CO2 cut --------------------------------------------------------

co2_cut_90_keep <- filter(co2_cut_joasete_90,
                  cut == "keep")  #to keep only the part we want to keep

# cleaning PAR --------------------------------------------------------------

# for ER we look at the range of PAR to see if there are errors
filter(co2_cut_90_keep, type == "ER") %>% #faster than looking at the graph!
  summarise(
    rangePAR = range(PAR)
  )

# visualize PAR levels

# filt_ER_90 <- filter(co2_cut_90_keep, type == "ER") # I am just filtering to make things easier
# plot(filt_ER_90$PAR) # Plot the PAR values
# plot(x= filt_ER_90$datetime, y= filt_ER_90$PAR) # Plot the PAR vs time
# unique(filt_ER_90[filt_ER_90$PAR>60,]$fluxID) # identify the weird values 
# range(filt_ER_90[filt_ER_90$PAR>60,]$PAR) # and the PAR levels (no big deal)
# unique(filt_ER_90[filt_ER_90$PAR>60,]$datetime) # who was on the field at this time...

# for ER we look at the range of PAR to see if there are errors
filter(co2_cut_90_keep, type == "ER") %>% #faster than looking at the graph!
  summarise(
    rangePAR = range(PAR)
  )

# check here in detail what happened in the plots with weird PAR values
filt_ER_90 %>% filter(fluxID == "227") %>% 
  ggplot(aes(x = datetime, y = PAR)) +
  geom_point()


# ... what should we do now??
# 1. think about weird PAR values. what could be happening, and how to solve it? (Discuss in class)
# 2. we should also manually modify the cuts for those curve that does not look fine with the automatic cuts.



# calculation of fluxes ---------------------------------------------------

cflux_joasete <- co2_cut_90_keep %>% 
  flux.calc.PFTC6()


# calculation of GPP ------------------------------------------------------

cflux_joasete_GPP <- cflux_joasete %>%
  mutate(
    pairID = case_when(
      type == "NEE" ~ fluxID,
      type == "ER" ~ fluxID-1
    )
  ) %>% 
  select(!c(p.value, r.squared, adj.r.squared, nobs)) %>% 
  # pivot_wider(names_from = type, values_from = PARavg, names_prefix = "PARavg_") %>% 
  # select(!c(PAR_corrected_flux)) %>%
  # select(campaign, turfID, date, type, corrected_flux) %>%
  pivot_wider(names_from = type, values_from = c(flux, temp_soilavg)) %>% 
  rename(
    ER = flux_ER,
    NEE = flux_NEE
  ) %>%
  mutate(
    GEP = NEE - ER
  ) %>% 
  pivot_longer(c(ER, NEE, GEP), names_to = "type", values_to = "corrected_flux") %>% 
  mutate(
    temp_soil = case_when(
      type == "ER" ~ temp_soilavg_ER,
      type == "NEE" ~ temp_soilavg_NEE,
      type == "GEP" ~ rowMeans(select(., c(temp_soilavg_NEE, temp_soilavg_ER)), na.rm = TRUE)
    )
  ) %>% 
  select(!c(temp_soilavg_ER, temp_soilavg_NEE))
  

write_csv(cflux_joasete, "clean_data/Three-D_24h-cflux_joasete_2022.csv")

