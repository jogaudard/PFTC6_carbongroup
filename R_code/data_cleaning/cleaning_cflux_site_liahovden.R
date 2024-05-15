#This script will be to separate c-flux data into turfIDs and clean the fluxes before we calculate them

# functions = dir(path = "R_code/functions", full.names = TRUE)
# sapply(functions, source)

# library("dataDownloader")

# download raw data
# download files from OSF ---------------------------------------

get_file(node = "fcbw4",
         file = "PFTC6_CO2_liahovden_2022.csv",
         path = "raw_data",
         remote_path = "raw_data/v. c_flux_raw_data")

get_file(node = "fcbw4",
         file = "PFTC6_cflux_field-record_liahovden.csv",
         path = "raw_data",
         remote_path = "raw_data/v. c_flux_raw_data")

# cleaning liahovden ------------------------------------------------------
# read the files
co2_24h_liahovden <- read_csv("raw_data/PFTC6_CO2_liahovden_2022.csv", na = c("#N/A"))

record_liahovden <- read_csv("raw_data/PFTC6_cflux_field-record_liahovden.csv", na = c(""))

# matching the CO2 concentration data with the turfs using the field record
# we have defined a default window length of 60 secs.
co2_fluxes_liahovden <- match.flux.PFTC6(co2_24h_liahovden, record_liahovden, startcrop = 10, measurement_length = 180, window_length = 160, date_format = "ymd")

# zhao18 cleaning ---------------------------------------------------------
slopes_zhao18_liahovden <- co2_fluxes_liahovden %>%
  filter(
    datetime > start_window &
      datetime < end_window
  ) %>%
  fitting.flux_nocut2(
    weird_fluxesID = c(43, 46, 86, 101)
  )

slopes_zhao18_metrics_liahovden <- slopes_zhao18_liahovden %>%
  select(fluxID, b, b_est, RMSE, tz, flag, cor_coef) %>%
  distinct()

# graph them
theme_set(theme_grey(base_size = 5))

slopes_zhao18_liahovden %>%
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
    "zero" = "grey"
  )) +
  scale_x_datetime(date_breaks = "1 min", minor_breaks = "10 sec", date_labels = "%e/%m \n %H:%M") +
  # ylim(400,800) +
  facet_wrap(~fluxID, scales = "free")

ggsave("liahovden1.png", height = 40, width = 100, units = "cm", path = "graph_fluxes")

gc()

# Graph them
slopes_zhao18_liahovden %>%
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
    "zero" = "grey"
  )) +
  scale_x_datetime(date_breaks = "1 min", minor_breaks = "10 sec", date_labels = "%e/%m \n %H:%M") +
  # ylim(400,700) +
  facet_wrap(~fluxID, scales = "free")

gc()

ggsave("liahovden2.png", height = 40, width = 100, units = "cm", path = "graph_fluxes")

# clean cut ---------------------------------------------------------------
# co2_cut_keep_liahovden <- filter(slopes_zhao18_liahovden,
#                        cut == "keep")  #to keep only the part we want to keep

co2_cut_keep_liahovden <- slopes_zhao18_liahovden

# cleaning PAR ------------------------------------------------------------
co2_cut_keep_liahovden <- co2_cut_keep_liahovden %>%
  mutate(
    PAR =
      case_when(
        type=="ER" & PAR <= 0 ~ 0,
        TRUE~PAR
      )
  )

filter(co2_cut_keep_liahovden, type == "NEE") %>% #faster than looking at the graph!
  summarise(
    rangePAR = range(PAR, na.rm = TRUE)
  )

# Graph them
co2_cut_keep_liahovden %>%
  filter(
    type == "NEE"
    # & PAR < 10
  ) %>%
  mutate(
    datetime = ymd_hms(datetime),
    time = hms::as_hms(datetime)
  ) %>%
  ggplot(aes(x = time, y = PAR)) +
  geom_point() +
  geom_text(aes(label = fluxID))

ggsave("PAR_NEE_liahovden.png", height = 30, width = 40, units = "cm", path = "graph_fluxes")

# calculation of fluxes ---------------------------------------------------

cflux_liahovden <- co2_cut_keep_liahovden %>%
  mutate(
    slope = case_when(
      flag == "ok" ~ slope_tz,
      flag == "zero" ~ 0,
      flag %in% c("discard", "start_error", "weird_flux") ~ NA_real_
    ),
    slope_noflag = slope_tz
    # slope = slope_tz # we keep the flags and the data
  ) %>%
  flux.calc.zhao18()

cflux_liahovden_GPP <- GPP.PFTC6(cflux_liahovden)

# correction and verification ---------------------------------------------

cflux_liahovden_corrected <- GPP_corr.PFTC6(cflux_liahovden_GPP,
                                            start_night = "22:58:00",
                                            end_night = "04:00:00",
                                            strategy = "max")

# Graph them
cflux_liahovden_corrected %>%
  filter(
    type != "NEE"
    # & turfID == "27 AN3C 27"
  ) %>%
  ggplot(aes(x = time, y = flux_corrected, color = type)) +
  geom_point()

ggsave("24h_liahovden.png", height = 30, width = 40, units = "cm", path = "graph_fluxes")


# write_csv(cflux_liahovden_corrected, "clean_data/PFTC6_24h-cflux_liahovden_2022.csv")
