
# This script will be to separate c-flux data into turfIDs and clean the fluxes before we calculate them

# source("code/functions.R")
# library("dataDownloader")
# library(tidyverse)
# library(lubridate)
# library(hms)

# download raw data
# download files from OSF ---------------------------------------

get_file(node = "fcbw4",
         file = "PFTC6_CO2_joasete_2022.csv",
         path = "raw_data",
         remote_path = "raw_data/c_flux_raw_data")

get_file(node = "fcbw4",
         file = "PFTC6_CO2_joasete_2_2022.csv",
         path = "raw_data",
         remote_path = "raw_data/c_flux_raw_data")

get_file(node = "fcbw4",
         file = "PFTC6_cflux_field-record_joasete.csv",
         path = "raw_data",
         remote_path = "raw_data/c_flux_raw_data")

# get_file(node = "fcbw4",
#          file = "PFTC6_cflux_cutting_joasete.csv",
#          path = "raw_data",
#          remote_path = "RawData/C-Flux")

# If you manage to download dataDownloader and download the data, you are good! Congrats!
# In case you did not manage to download the data manually. Call me :-)

# cleaning Vikesland ------------------------------------------------------
# read the files
# Joasete was done in two times because of a licor failure
co2_24h_joasete <- read_csv("raw_data/PFTC6_CO2_joasete_2022.csv", na = c("#N/A"), col_types = "cfdddddd") %>% 
  bind_rows(
    read_csv("raw_data/PFTC6_CO2_joasete_2_2022.csv", na = c("#N/A"), col_types = "cfdddddd")
  )
  
record_joasete <- read_csv("raw_data/PFTC6_cflux_field-record_joasete.csv", na = c("")) %>% 
  # mutate(
  #   starting_time = as.double(starting_time)
  # ) %>%
  filter( # we need to remove the fluxes that were measured when the licor was broken
    !(starting_time %in% c("032158", "032538", "032951", "033348", "033919")
    & date == "2022-07-29")
  ) 




# matching the CO2 concentration data with the turfs using the field record
# we have defined a default window length of 90 secs.

co2_fluxes_joasete <- match.flux.PFTC6(co2_24h_joasete, record_joasete, startcrop = 10, window_length = 160, measurement_length = 180, date_format = "ymd")
# at Joasete, turfID 29 WN3C 106 and 109 AN3C 109 were swapped between 2022-07-28 11:20:00 and 2022-07-29 00:30:00

co2_fluxes_joasete <- co2_fluxes_joasete %>% 
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


# zhao18 method -----------------------------------------------------------

slopes_zhao18_joasete <- co2_fluxes_joasete %>% 
  filter(
    datetime > start_window &
      datetime < end_window
  ) %>% 
  fitting.flux_nocut2(
    weird_fluxesID = c(41,73,77)
  )

slopes_zhao18_metrics_joasete <- slopes_zhao18_joasete %>% 
  select(fluxID, b, b_est, RMSE, tz, flag, cor_coef) %>% 
  distinct()

# graph them
theme_set(theme_grey(base_size = 5))

slopes_zhao18_joasete %>% 
  filter(
    fluxID %in% c(1:100)
  ) %>% 
  ggplot(aes(datetime)) +
  geom_point(aes(y = CO2), size = 0.2) +
  geom_line(aes(y = fit), linetype = "longdash") +
  geom_line(aes(y = fit_slope, color = flag), linetype = "dashed") +
  scale_color_manual(values = c(
    "keep" = "green",
    "cut" = "red",
    "ok" = "black",
    "discard" = "red",
    "zero" = "grey",
    "start_error" = "red"
  )) +
  scale_x_datetime(date_breaks = "1 min", minor_breaks = "10 sec", date_labels = "%e/%m \n %H:%M") +
  # ylim(400,800) +
  facet_wrap(~fluxID, scales = "free")

ggsave("joasete1.png", height = 40, width = 100, units = "cm", path = "graph_fluxes")

gc()

# Graph them
slopes_zhao18_joasete %>% 
  filter(
    fluxID %in% c(101:200)
  ) %>% 
  ggplot(aes(datetime)) +
  geom_point(aes(y = CO2), size = 0.2) +
  geom_line(aes(y = fit), linetype = "longdash") +
  geom_line(aes(y = fit_slope, color = flag), linetype = "dashed") +
  scale_color_manual(values = c(
    "keep" = "green",
    "cut" = "red",
    "ok" = "black",
    "discard" = "red",
    "zero" = "grey",
    "start_error" = "red"
  )) +
  scale_x_datetime(date_breaks = "1 min", minor_breaks = "10 sec", date_labels = "%e/%m \n %H:%M") +
  # ylim(400,700) +
  facet_wrap(~fluxID, scales = "free")

gc()

ggsave("joasete2.png", height = 40, width = 100, units = "cm", path = "graph_fluxes")

# Graph them

slopes_zhao18_joasete %>% 
  filter(
    fluxID %in% c(201:300)
  ) %>% 
  ggplot(aes(datetime)) +
  geom_point(aes(y = CO2), size = 0.2) +
  geom_line(aes(y = fit), linetype = "longdash") +
  geom_line(aes(y = fit_slope, color = flag), linetype = "dashed") +
  scale_color_manual(values = c(
    "keep" = "green",
    "cut" = "red",
    "ok" = "black",
    "discard" = "red",
    "zero" = "grey",
    "start_error" = "red"
  )) +
  scale_x_datetime(date_breaks = "1 min", minor_breaks = "10 sec", date_labels = "%e/%m \n %H:%M") +
  # ylim(400,700) +
  facet_wrap(~fluxID, scales = "free")

gc()

ggsave("joasete3.png", height = 40, width = 100, units = "cm", path = "graph_fluxes")

# at that stage, you should visually check the fluxes before going on with the calculations

# clean cut ---------------------------------------------------------------

# co2_cut_keep_joasete <- filter(slopes_zhao18_joasete,
#                        cut == "keep")  #to keep only the part we want to keep

co2_cut_keep_joasete <- slopes_zhao18_joasete


# cleaning PAR ------------------------------------------------------------

co2_cut_keep_joasete <- co2_cut_keep_joasete %>% 
  mutate(
    PAR =
      case_when(
        type=="ER" & PAR <= 0 ~ 0, 
        TRUE~PAR
      )
  )

filter(co2_cut_keep_joasete, type == "NEE") %>% #faster than looking at the graph!
  summarise(
    rangePAR = range(PAR, na.rm = TRUE)
  )

# Graph them
co2_cut_keep_joasete %>% 
  filter(
    type == "NEE"
    # & fluxID == 95
    # & PAR < 10
  ) %>% 
  mutate(
    datetime = ymd_hms(datetime),
    time = hms::as_hms(datetime)
  ) %>% 
  ggplot(aes(x = time, y = PAR)) +
  geom_point() +
  geom_text(aes(label = fluxID))

ggsave("PAR_NEE_joasete.png", height = 30, width = 40, units = "cm", path = "graph_fluxes")



# calculation of fluxes ---------------------------------------------------

cflux_joasete <- co2_cut_keep_joasete %>% 
  mutate(
    slope = case_when(
      flag == "ok" ~ slope_tz,
      flag == "zero" ~ 0,
      flag %in% c("discard", "start_error", "weird_flux") ~ NA_real_
    ),
    slope_noflag = slope_tz
  ) %>%
  flux.calc.zhao18()

cflux_joasete_GPP <- GPP.PFTC6(cflux_joasete)


# correction and verification ---------------------------------------------

cflux_joasete_corrected <- GPP_corr.PFTC6(cflux_joasete_GPP,
                                            start_night = "23:00:00",
                                            end_night = "04:00:00",
                                            strategy = "max")

# Graph them
cflux_joasete_corrected %>%
  filter(
    type != "NEE"
    # & turfID == "27 AN3C 27"
  ) %>%
  ggplot(aes(x = time, y = flux_corrected, color = type)) +
  geom_point()

ggsave("24h_joasete.png", height = 30, width = 40, units = "cm", path = "graph_fluxes")


# write_csv(cflux_joasete_corrected, "clean_data/PFTC6_24h-cflux_liahovden_2022.csv")

# previous method ---------------------------------------------------------



# # cutting Vikesland ------------------------------------------------------
# cutting_joasete <- read_csv("raw_data/PFTC6_cflux_cutting_joasete.csv", na = "", col_types = "dtt")
# 
# co2_cut_joasete <- co2_fluxes_joasete %>% 
#   left_join(cutting_joasete, by = "fluxID") %>% 
#   mutate(
#     start_cut = ymd_hms(paste(date, .$start_cut)),
#     end_cut = ymd_hms(paste(date, .$end_cut))
#   )
# 
# # adjusting the time window with manual cuts ------------------------------------------------------
# 
# co2_cut_joasete <- co2_cut_joasete %>%
#   mutate(
#   start_window = case_when(
#     is.na(start_cut) == FALSE ~ start_cut,
#     TRUE ~ start_window
#   ),
#   end_window = case_when(
#     is.na(end_cut) == FALSE ~ end_cut,
#     TRUE ~ end_window
#   ),
#   cut = case_when(
#     datetime <= start_window | datetime >= end_window ~ "cut",
#     fluxID == 163 ~ "cut",
#     fluxID == 164 ~ "cut",
#     # fluxID ==  & datetime %in%  ~ "cut",
#     TRUE ~ "keep"
#     ),
#   cut = as_factor(cut)
#   )
# 
# 
# # vizz Vikesland -------------------------------------------------------
# 
# # visualizing 90 secs cuts in Vikesland (it´s in comments, just in case you don´t want to visualize it)
# 
# # theme_set(theme_grey(base_size = 5))
# # 
# # co2_cut_joasete %>%
# #   ggplot(aes(x = datetime, y = CO2, colour = cut)) +
# #   geom_line(size = 0.2, aes(group = fluxID)) +
# #   # geom_line(size = 0.2) +
# #   scale_x_datetime(date_breaks = "1 min", minor_breaks = "10 sec", date_labels = "%e/%m \n %H:%M") +
# #   # scale_x_date(date_labels = "%H:%M:%S") +
# #   facet_wrap(vars(fluxID), ncol = 30, scales = "free")
# # 
# # ggsave("fluxes_details_joasete.png", height = 40, width = 80, units = "cm")
# 
# theme_set(theme_grey(base_size = 5))
# # 
# co2_cut_joasete %>%
#   mutate(
#     fluxID = as.numeric(fluxID)
#   ) %>% 
#   filter(
#     fluxID %in% c(155, 156)
#   ) %>% 
#   ggplot(aes(x = datetime, y = CO2, colour = cut)) +
#   geom_line(size = 0.2, aes(group = fluxID)) +
#   # geom_line(size = 0.2) +
#   scale_x_datetime(date_breaks = "1 min", minor_breaks = "10 sec", date_labels = "%e/%m \n %H:%M") +
#   # scale_x_date(date_labels = "%H:%M:%S") +
#   facet_wrap(vars(fluxID), ncol = 2, scales = "free")
# 
# 
# # produce clean CO2 cut --------------------------------------------------------
# 
# co2_cut_keep <- filter(co2_cut_joasete,
#                   cut == "keep")  #to keep only the part we want to keep
# 
# # cleaning PAR --------------------------------------------------------------
# 
# # for ER we look at the range of PAR to see if there are errors
# filter(co2_cut_keep, type == "ER") %>% #faster than looking at the graph!
#   summarise(
#     rangePAR = range(PAR)
#   )
# 
# filter(co2_cut_keep, type == "NEE") %>% #faster than looking at the graph!
#   summarise(
#     rangePAR = range(PAR)
#   )
# 
# co2_cut_keep %>% 
#   filter(
#     type == "NEE"
#   ) %>% 
#   mutate(
#     datetime = ymd_hms(datetime),
#     time = hms::as_hms(datetime)
#   ) %>% 
#   ggplot(aes(x = time, y = PAR)) +
#   geom_point()
# #negative PAR for ER should be 0
# co2_cut_keep <- co2_cut_keep %>% 
#   mutate(
#     PAR = case_when(
#       type == "ER" & PAR < 0 ~ 0,
#       TRUE ~ PAR
#     )
#   )
# 
# # visualize PAR levels
# 
# # filt_ER_90 <- filter(co2_cut_90_keep, type == "ER") # I am just filtering to make things easier
# # plot(filt_ER_90$PAR) # Plot the PAR values
# # plot(x= filt_ER_90$datetime, y= filt_ER_90$PAR) # Plot the PAR vs time
# # unique(filt_ER_90[filt_ER_90$PAR>60,]$fluxID) # identify the weird values 
# # range(filt_ER_90[filt_ER_90$PAR>60,]$PAR) # and the PAR levels (no big deal)
# # unique(filt_ER_90[filt_ER_90$PAR>60,]$datetime) # who was on the field at this time...
# 
# # for ER we look at the range of PAR to see if there are errors
# # filter(co2_cut_90_keep, type == "ER") %>% #faster than looking at the graph!
# #   summarise(
# #     rangePAR = range(PAR)
# #   )
# 
# # check here in detail what happened in the plots with weird PAR values
# # filt_ER_90 %>% filter(fluxID == "227") %>% 
#   # ggplot(aes(x = datetime, y = PAR)) +
#   # geom_point()
# 
# 
# # ... what should we do now??
# # 1. think about weird PAR values. what could be happening, and how to solve it? (Discuss in class)
# # 2. we should also manually modify the cuts for those curve that does not look fine with the automatic cuts.
# 
# 
# 
# # calculation of fluxes ---------------------------------------------------
# 
# cflux_joasete <- co2_cut_keep %>% 
#   flux.calc.PFTC6()
# 
# 
# # calculation of GPP ------------------------------------------------------
# 
# # cflux_joasete_GPP <- cflux_joasete %>%
# #   mutate(
# #     pairID = case_when(
# #       type == "NEE" ~ fluxID,
# #       type == "ER" ~ fluxID-1
# #     )
# #   ) %>% 
# #   select(!c(p.value, r.squared, adj.r.squared, nobs)) %>% 
# #   # pivot_wider(names_from = type, values_from = PARavg, names_prefix = "PARavg_") %>% 
# #   # select(!c(PAR_corrected_flux)) %>%
# #   # select(campaign, turfID, date, type, corrected_flux) %>%
# #   pivot_wider(names_from = type, values_from = c(flux, temp_soilavg)) %>% 
# #   rename(
# #     ER = flux_ER,
# #     NEE = flux_NEE
# #   ) %>%
# #   mutate(
# #     GEP = NEE - ER
# #   ) %>% 
# #   pivot_longer(c(ER, NEE, GEP), names_to = "type", values_to = "corrected_flux") %>% 
# #   mutate(
# #     temp_soil = case_when(
# #       type == "ER" ~ temp_soilavg_ER,
# #       type == "NEE" ~ temp_soilavg_NEE,
# #       type == "GEP" ~ rowMeans(select(., c(temp_soilavg_NEE, temp_soilavg_ER)), na.rm = TRUE)
# #     )
# #   ) %>% 
# #   select(!c(temp_soilavg_ER, temp_soilavg_NEE))
# 
# 
# # pvalue and R2 rule ------------------------------------------------------
# p = 0.01
# R2 = 0.7
# 
# cflux_joasete_clean <- cflux_joasete %>% 
#   mutate(
#   flux = case_when(
#     "p.value" > p  & adj.r.squared < R2 & type == "NEE" ~ 0,
#     "p.value" > p  & adj.r.squared < R2 & type == "ER" ~ NA_real_,
#     "p.value" <= p & "adj.r.squared" < R2 ~ NA_real_,
#     "p.value" > p & "adj.r.squared" >= R2 ~ flux,
#     "p.value" <= p & "adj.r.squared" >= R2 ~ flux
#     # , TRUE ~ flux
#   ))
# 
# cflux_joasete_GPP <- GPP.PFTC6(cflux_joasete)
# cflux_joasete_GPP_clean <- GPP.PFTC6(cflux_joasete_clean)
# 
# cflux_joasete_GPP_clean %>% 
#   filter(type == "GPP") %>% 
# count(
#   flux > 0
# )
# 
# 
# # verifying data ----------------------------------------------------------
# 
# cflux_joasete_GPP %>% 
#   filter(
#     type != "NEE"
#   ) %>% 
#   ggplot(aes(x = datetime, y = flux, color = type)) +
#   geom_point() +
#   # geom_text(aes(label = turfID)) +
#   scale_x_datetime(date_breaks = "2 hours", minor_breaks = "30 min", date_labels = "%e/%m \n %H:%M")
#   
# # cflux_joasete_GPP_clean %>% 
# #   mutate(
# #     datetime = ymd_hms(datetime),
# #     time = as_hms(datetime)
# #   ) %>% 
# #   filter(
# #     type != "NEE"
# #   ) %>%
# #   ggplot(aes(x = time, y = flux, color = type)) +
# #   geom_point() 
#   # geom_text(aes(label = turfID)) +
#   # scale_x_datetime(date_breaks = "2 hours", minor_breaks = "30 min", date_labels = "%H:%M")
# 
# 
# # GPP correction factor ---------------------------------------------------
# 
# # NEE is higher than it should be because it is the first measurement and it is measuring the CO2 accumulated in the canopy.
# # We will calculate a correction factor for GPP by assuming it is equal to 0 between 2300 and 0400 (nautical twilight)
# 
# # GPP_corr.PFTC6 <- function(fluxes, start_night = "23:00:00", end_night = "04:00:00", strategy = c("mean", "max")){
# #   fluxes_corr <- fluxes %>% 
# #   mutate(
# #     datetime = ymd_hms(datetime),
# #     time = hms::as_hms(datetime),
# #     # time = hms(time),
# #     type = as_factor(type),
# #     difference = case_when(
# #       type == "GPP" 
# #       & (time >= hms(start_night) | time <= hms(end_night))
# #       & flux > 0 
# #       ~ flux
# #     ),
# #     corr_factor = case_when(
# #       strategy == "mean" ~ mean(difference, na.rm = TRUE),
# #       strategy == "max" ~ max(difference, na.rm = TRUE)
# #     ),
# #     flux_corrected = case_when(
# #       type == "ER" ~ flux,
# #       type == "GPP" ~ flux - corr_factor,
# #       type == "NEE" ~ flux - corr_factor
# #     )
# #   )
# #   
# #   return(fluxes_corr)
# # }
# 
# cflux_joasete_corrected <- GPP_corr.PFTC6(cflux_joasete_GPP_clean,
#                                           start_night = "23:00:00",
#                                           end_night = "04:00:00",
#                                           strategy = "max")
# 
# cflux_joasete_corrected %>%
#   filter(
#     type != "NEE"
#   ) %>%
#   ggplot(aes(x = time, y = flux_corrected, color = type)) +
#   geom_point()
#   # geom_point(size = 0.01) +
#   # geom_text(aes(label = turfID))
#   # scale_x_datetime(date_breaks = "2 hours", minor_breaks = "30 min", date_labels = "%e/%m \n %H:%M")
# 
# 
#   
# 
# write_csv(cflux_joasete_corrected, "clean_data/PFTC6_24h-cflux_joasete_2022.csv")
# 
