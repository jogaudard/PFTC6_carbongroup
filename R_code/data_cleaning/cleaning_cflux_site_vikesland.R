# This script will be to separate c-flux data into turfIDs and clean the fluxes before we calculate them

# functions = dir(path = "R_code/functions", full.names = TRUE)
# sapply(functions, source)
#
# library("dataDownloader")
# library("tidyverse")
# library("scales")

# download raw data
# download files from OSF ---------------------------------------
get_file(node = "fcbw4",
         file = "PFTC6_CO2_vikesland_2022.csv",
         path = "raw_data",
         remote_path = "raw_data/v. c_flux_raw_data")

get_file(node = "fcbw4",
         file = "PFTC6_cflux_field-record_vikesland.csv",
         path = "raw_data",
         remote_path = "raw_data/v. c_flux_raw_data")

# cleaning Vikesland ------------------------------------------------------
# read the files
co2_24h_vikesland <- read_csv("raw_data/PFTC6_CO2_vikesland_2022.csv", na = c("#N/A"))

record_vikesland <- read_csv("raw_data/PFTC6_cflux_field-record_vikesland.csv", na = c(""))

# matching the CO2 concentration data with the turfs using the field record
# we have defined a default window length of 60 secs.
co2_fluxes_vikesland <- match.flux.PFTC6(co2_24h_vikesland, record_vikesland, startcrop = 10, window_length = 160, measurement_length = 180)

# zhao18 method -----------------------------------------------------------
slopes_zhao18_vikesland <- co2_fluxes_vikesland %>%
  filter(
    datetime > start_window &
      datetime < end_window
  ) %>%
  fitting.flux_nocut2(
    weird_fluxesID = c(73, 141)
  )

slopes_zhao18_metrics_vikesland <- slopes_zhao18_vikesland %>%
  select(fluxID, b, b_est, RMSE, tz, flag, cor_coef) %>%
  distinct()

# graph them
theme_set(theme_grey(base_size = 5))

slopes_zhao18_vikesland %>%
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

ggsave("vikesland1.png", height = 40, width = 100, units = "cm", path = "graph_fluxes")

gc()

# Graph them
slopes_zhao18_vikesland %>%
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

ggsave("vikesland2.png", height = 40, width = 100, units = "cm", path = "graph_fluxes")

#Graph them
slopes_zhao18_vikesland %>%
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

ggsave("vikesland3.png", height = 40, width = 100, units = "cm", path = "graph_fluxes")

# at that stage, you should visually check the fluxes before going on with the calculations

# clean cut ---------------------------------------------------------------
# co2_cut_keep_vikesland <- filter(slopes_zhao18_vikesland,
#                        cut == "keep")  #to keep only the part we want to keep

co2_cut_keep_vikesland <- slopes_zhao18_vikesland

# cleaning PAR ------------------------------------------------------------
co2_cut_keep_vikesland <- co2_cut_keep_vikesland %>%
  mutate(
    PAR =
      case_when(
        type=="ER" & PAR <= 0 ~ 0,
        TRUE~PAR
      )
  )

# there were some PAR sensor failures
co2_cut_keep_vikesland <- co2_cut_keep_vikesland %>%
  mutate(
    PAR = case_when(
      fluxID %in% c(143, 153, 155, 177, 179, 151)
      & PAR < 10
      ~ NA_real_,
      TRUE ~ PAR
    )
  )

filter(co2_cut_keep_vikesland, type == "NEE") %>% #faster than looking at the graph!
  summarise(
    rangePAR = range(PAR, na.rm = TRUE)
  )

# Graph them
co2_cut_keep_vikesland %>%
  filter(
    type == "NEE"
    # & fluxID %in% c(151
      # 143
      # 153
      # 155
      # 177
      # 179
      # )
    # & PAR < 5
  ) %>%
  mutate(
    datetime = ymd_hms(datetime),
    time = hms::as_hms(datetime)
  ) %>%
  ggplot(aes(x = time, y = PAR)) +
  geom_point() +
  geom_text(aes(label = fluxID))

ggsave("PAR_NEE_vikesland.png", height = 30, width = 40, units = "cm", path = "graph_fluxes")

# calculation of fluxes ---------------------------------------------------
cflux_vikesland <- co2_cut_keep_vikesland %>%
  mutate(
    slope = case_when(
      flag == "ok" ~ slope_tz,
      flag == "zero" ~ 0,
      flag %in% c("discard", "start_error", "weird_flux") ~ NA_real_
    ),
    slope_noflag = slope_tz
  ) %>%
  flux.calc.zhao18()

cflux_vikesland_GPP <- GPP.PFTC6(cflux_vikesland)

# correction and verification ---------------------------------------------
cflux_vikesland_corrected <- GPP_corr.PFTC6(cflux_vikesland_GPP,
                                          start_night = "22:30:00", # we expand the window because there still are some positive GPP values just at the edge of the usual window
                                          end_night = "04:30:00",
                                          strategy = "max")
# Graph them
cflux_vikesland_corrected %>%
  filter(
    type != "NEE"
    # & turfID == "27 AN3C 27"
  ) %>%
  ggplot(aes(x = time, y = flux_corrected, color = type, label = time)) +
  # geom_text() +
  # scale_x_datetime(date_breaks = "5 hour", minor_breaks = "1 hour", date_labels = "%e/%m \n %H:%M") +
  geom_point()

ggsave("24h_vikesland.png", height = 30, width = 40, units = "cm", path = "graph_fluxes")

# write_csv(cflux_joasete_corrected, "clean_data/PFTC6_24h-cflux_liahovden_2022.csv")
