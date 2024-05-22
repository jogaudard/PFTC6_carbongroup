# This script will be to separate c-flux data into turfIDs and clean the fluxes before we calculate them

# functions = dir(path = "R_code/functions", full.names = TRUE)
# sapply(functions, source)
#
# library("dataDownloader")

# download raw data
# download files from OSF ---------------------------------------

get_file(node = "fcbw4",
         file = "PFTC6_CO2_hogsete_2022.csv",
         path = "raw_data",
         remote_path = "raw_data/v. c_flux_raw_data")

get_file(node = "fcbw4",
         file = "PFTC6_cflux_field-record_hogsete.csv",
         path = "raw_data",
         remote_path = "raw_data/v. c_flux_raw_data")

# cleaning hogsete ------------------------------------------------------
# read the files
co2_24h_hogsete <- read_csv("raw_data/PFTC6_CO2_hogsete_2022.csv", na = c("#N/A"))

record_hogsete <- read_csv("raw_data/PFTC6_cflux_field-record_hogsete.csv", na = c(""))
record_hogsete$turfID <- sub("^", "TTC ", record_hogsete$turfID)

# matching the CO2 concentration data with the turfs using the field record
# we have defined a default window length of 60 secs.
co2_fluxes_hogsete <- match.flux.PFTC6(co2_24h_hogsete, record_hogsete, startcrop = 10, window_length = 160, measurement_length = 180, date_format = "ymd")

# zhao18 method -----------------------------------------------------------

slopes_zhao18_hogsete <- co2_fluxes_hogsete %>%
  filter(
    datetime > start_window &
      datetime < end_window
  ) %>%
  # fitting.flux_()
  fitting.flux_nocut2()

slopes_zhao18_metrics_hogsete <- slopes_zhao18_hogsete %>%
  select(fluxID, b, b_est, RMSE, a, tz, Cm, flag, cor_coef) %>%
  distinct()

# graph them
theme_set(theme_grey(base_size = 5))

slopes_zhao18_hogsete %>%
  filter(
    fluxID %in% c(1:100)
  ) %>%
  ggplot(aes(datetime)) +
  geom_point(aes(y = CO2), size = 0.2) +
  geom_line(aes(y = fit), linetype = "longdash") +
  geom_line(aes(y = fit_slope, color = flag), linetype = "dashed") +
  scale_color_manual(values = c(
    # "keep" = "green",
    # "cut" = "red",
    "ok" = "black",
    "discard" = "red",
    "zero" = "grey",
    "start_error" = "red"
  )) +
  scale_x_datetime(date_breaks = "1 min", minor_breaks = "10 sec", date_labels = "%e/%m \n %H:%M") +
  # ylim(400,800) +
  facet_wrap(~fluxID, scales = "free")

ggsave("hogsete1.png", height = 40, width = 100, units = "cm", path = "graph_fluxes")

gc()

# Graph them
slopes_zhao18_hogsete %>%
  filter(
    fluxID %in% c(101:200)
  ) %>%
  ggplot(aes(datetime)) +
  geom_point(aes(y = CO2), size = 0.2) +
  geom_line(aes(y = fit), linetype = "longdash") +
  geom_line(aes(y = fit_slope, color = flag), linetype = "dashed") +
  scale_color_manual(values = c(
    # "keep" = "green",
    # "cut" = "red",
    "ok" = "black",
    "discard" = "red",
    "zero" = "grey",
    "start_error" = "red"
  )) +
  scale_x_datetime(date_breaks = "1 min", minor_breaks = "10 sec", date_labels = "%e/%m \n %H:%M") +
  # ylim(400,500) +
  facet_wrap(~fluxID, scales = "free")

gc()

ggsave("hogsete2.png", height = 40, width = 100, units = "cm", path = "graph_fluxes")

# slopes_zhao18 %>%
#   filter(
#     fluxID %in% c(201:300)
#   ) %>%
#   ggplot(aes(datetime)) +
#   geom_point(aes(y = CO2, color = cut), size = 0.2) +
#   geom_line(aes(y = fit), linetype = "longdash") +
#   geom_line(aes(y = fit_slope, color = flag), linetype = "dashed") +
#   scale_color_manual(values = c(
#     "keep" = "green",
#     "cut" = "red",
#     "ok" = "black",
#     "discard" = "red",
#     "zero" = "grey",
#     "start_error" = "red"
#   )) +
#   scale_x_datetime(date_breaks = "1 min", minor_breaks = "10 sec", date_labels = "%e/%m \n %H:%M") +
#   # ylim(400,700) +
#   facet_wrap(~fluxID, scales = "free")
#
# gc()
#
# ggsave("hogsete3.png", height = 40, width = 100, units = "cm")

# at that stage, you should visually check the fluxes before going on with the calculations

# clean cut ---------------------------------------------------------------

# co2_cut_keep_hogsete <- filter(slopes_zhao18_hogsete,
#                        cut == "keep")  #to keep only the part we want to keep

co2_cut_keep_hogsete <- slopes_zhao18_hogsete

# cleaning PAR ------------------------------------------------------------

co2_cut_keep_hogsete <- co2_cut_keep_hogsete %>%
  mutate(
    PAR =
      case_when(
        type=="ER" & PAR <= 0 ~ 0,
        TRUE~PAR
      )
  )

filter(co2_cut_keep_hogsete, type == "NEE") %>% #faster than looking at the graph!
  summarise(
    rangePAR = range(PAR, na.rm = TRUE)
  )

# Graph the above
co2_cut_keep_hogsete %>%
  filter(
    type == "NEE"
    # & fluxID == 47
    # & PAR < 10
  ) %>%
  mutate(
    datetime = ymd_hms(datetime),
    time = hms::as_hms(datetime)
  ) %>%
  ggplot(aes(x = time, y = PAR)) +
  geom_point() +
  geom_text(aes(label = fluxID))

ggsave("PAR_NEE_hogsete.png", height = 30, width = 40, units = "cm", path = "graph_fluxes")

# calculation of fluxes ---------------------------------------------------
cflux_hogsete <- co2_cut_keep_hogsete %>%
  mutate(
    slope = case_when(
      flag == "ok" ~ slope_tz,
      flag == "zero" ~ 0,
      flag %in% c("discard", "start_error", "weird_flux") ~ NA_real_
    ),
    slope_noflag = slope_tz
  ) %>%
  flux.calc.zhao18()

cflux_hogsete_GPP <- GPP.PFTC6(cflux_hogsete)

# correction and verification ---------------------------------------------
cflux_hogsete_corrected <- GPP_corr.PFTC6(cflux_hogsete_GPP,
                                          start_night = "23:00:00",
                                          end_night = "04:00:00",
                                          strategy = "max")

# Graph this
cflux_hogsete_corrected %>%
  filter(
    type != "NEE"
    # & turfID == "27 AN3C 27"
  ) %>%
  ggplot(aes(x = time, y = flux_corrected, color = type)) +
  geom_point()

ggsave("24h_hogsete.png", height = 30, width = 40, units = "cm", path = "graph_fluxes")

# write_csv(cflux_hogsete_corrected, "clean_data/PFTC6_24h-cflux_liahovden_2022.csv")
