# This script will be to separate c-flux data into turfIDs and clean the fluxes before we calculate them

# functions = dir(path = "R_code/functions", full.names = TRUE)
# sapply(functions, source)

# library("dataDownloader")
# library(tidyverse)
# library(lubridate)
# library(hms)

# download raw data
# download files from OSF ---------------------------------------

get_file(node = "fcbw4",
         file = "PFTC6_CO2_joasete_2022.csv",
         path = "raw_data",
         remote_path = "raw_data/v. c_flux_raw_data")

get_file(node = "fcbw4",
         file = "PFTC6_CO2_joasete_2_2022.csv",
         path = "raw_data",
         remote_path = "raw_data/v. c_flux_raw_data")

get_file(node = "fcbw4",
         file = "PFTC6_cflux_field-record_joasete.csv",
         path = "raw_data",
         remote_path = "raw_data/v. c_flux_raw_data")

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
