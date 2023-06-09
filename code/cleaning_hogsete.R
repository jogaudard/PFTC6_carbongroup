
# This script will be to separate c-flux data into turfIDs and clean the fluxes before we calculate them

# source("code/functions.R")
# 
# library("dataDownloader")

# download raw data
# download files from OSF ---------------------------------------

get_file(node = "fcbw4",
         file = "PFTC6_CO2_hogsete_2022.csv",
         path = "raw_data",
         remote_path = "raw_data/c_flux_raw_data")

get_file(node = "fcbw4",
         file = "PFTC6_cflux_field-record_hogsete.csv",
         path = "raw_data",
         remote_path = "raw_data/c_flux_raw_data")

# get_file(node = "pk4bg",
#          file = "PFTC6_cflux_cutting_hogsete.csv",
#          path = "raw_data",
#          remote_path = "RawData/C-Flux")

# If you manage to download dataDownloader and download the data, you are good! Congrats!
# In case you did not manage to download the data manually. Call me :-)

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
    )
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


# previous method ---------------------------------------------------------

# 
# # cutting hogsete ------------------------------------------------------
# # cutting_hogsete <- read_csv("raw_data/PFTC6_cflux_cutting_hogsete.csv", na = "")
# #cutting_hogsete <- read_csv("raw_data/PFTC6_cflux_cutting_hogsete.csv", na = "", col_types = "dtt") # I removed the last part of the line
# 
# # cutting_hogsete$start_cut <- gsub("(\\d{2})(?=\\d{2})", "\\1:", cutting_hogsete$start_cut, perl = TRUE) # to add the : in the time
# # cutting_hogsete$end_cut <- gsub("(\\d{2})(?=\\d{2})", "\\1:", cutting_hogsete$end_cut, perl = TRUE) # to add the : in the time
# 
# cutting_hogsete <- tibble( #bypass manual cuts
#   fluxID = c(1:5),
#   start_cut = NA,
#   end_cut = NA
# )
# 
# co2_cut_hogsete <- co2_fluxes_hogsete %>% 
#   left_join(cutting_hogsete, by = "fluxID") %>% 
#   mutate(
#     start_cut = ymd_hms(paste(date, .$start_cut)),
#     end_cut = ymd_hms(paste(date, .$end_cut))
#   )
# 
# # adjusting the time window with manual cuts ------------------------------------------------------
# 
# co2_cut_hogsete <- co2_cut_hogsete %>%
#   mutate(
#     start_window = case_when(
#       is.na(start_cut) == FALSE ~ start_cut,
#       TRUE ~ start_window
#     ),
#     end_window = case_when(
#       is.na(end_cut) == FALSE ~ end_cut,
#       TRUE ~ end_window
#     ),
#     cut = case_when(
#       datetime <= start_window | datetime >= end_window ~ "cut",
#       fluxID ==  33  ~ "cut",
#       TRUE ~ "keep"
#     ),
#     cut = as_factor(cut)
#   )
# 
# # vizz hogsete -------------------------------------------------------
# 
# # visualizing 60 secs cuts in hogsete (it´s in comments, just in case you don´t want to visualize it)
# 
#  # theme_set(theme_grey(base_size = 5))
#  # 
#  # co2_cut_hogsete_60 %>%
#  #   ggplot(aes(x = datetime, y = CO2, colour = cut)) +
#  #   geom_line(size = 0.2, aes(group = fluxID)) +
#  #   # geom_line(size = 0.2) +
#  #   scale_x_datetime(date_breaks = "1 min", minor_breaks = "10 sec", date_labels = "%e/%m \n %H:%M") +
#  #   # scale_x_date(date_labels = "%H:%M:%S") +
#  #   facet_wrap(vars(fluxID), ncol = 30, scales = "free")
#  # 
#  # ggsave("fluxes_details_hogsete.png", height = 40, width = 80, units = "cm")
# 
# 
# # produce clean CO2 cut --------------------------------------------------------
# 
# co2_cut_keep <- filter(co2_cut_hogsete,
#                           cut == "keep")  #to keep only the part we want to keep
# 
# # cleaning PAR --------------------------------------------------------------
# 
# # for ER we look at the range of PAR to see if there are errors
# # filter(co2_cut_60_keep, type == "ER") %>% #faster than looking at the graph!
# #   summarise(
# #     rangePAR = range(PAR)
#   # )
# 
# # visualize PAR levels
# 
# # filt_ER_60 <- filter(co2_cut_60_keep, type == "ER") # I am just filtering to make things easier
# # 
# # plot(filt_ER_60$PAR) # Plot the PAR values
# # plot(x= filt_ER_60$datetime, y= filt_ER_60$PAR) # Plot the PAR vs time
# # abline(h=0, col="red")
# 
# # now we are replacing negative PAR values in type=ER by zero values.
# 
# co2_cut_keep <- co2_cut_keep %>% 
#   mutate(
#     PAR =
#       case_when(
#         type=="ER" & PAR <= 0 ~ 0, 
#         TRUE~PAR
#       )
#   )
# 
# filter(co2_cut_keep, type == "NEE") %>% #faster than looking at the graph!
#   summarise(
#     rangePAR = range(PAR, na.rm = TRUE)
#   )
# 
# co2_cut_keep %>% 
#   filter(
#     type == "NEE"
#     & PAR < 0
#   ) %>% 
#   mutate(
#     datetime = ymd_hms(datetime),
#     time = hms::as_hms(datetime)
#   ) %>% 
#   ggplot(aes(x = time, y = PAR)) +
#   geom_point(size = 0.01) +
#   geom_text(aes(label = fluxID))
# 
# co2_cut_keep %>%
#   mutate(
#     fluxID = as.numeric(fluxID)
#   ) %>%
#   filter(
#     fluxID %in% c(3)
#   ) %>%
#   mutate(
#     datetime = ymd_hms(datetime),
#     time = hms::as_hms(datetime)
#   ) %>%
#   ggplot(aes(x = time, y = PAR)) +
#   geom_point() +
#   facet_wrap(vars(fluxID), scales = "free")
# # 
# # 
# co2_cut_keep <- co2_cut_keep %>%
#   mutate(
#     fluxID = as.numeric(fluxID),
#     PAR =
#       case_when(
#         fluxID %in% c(3)
#         # | (fluxID %in% c() & PAR < 0)
#         # | (fluxID ==  & PAR < 50)
#         ~ NA_real_,
#         TRUE ~ PAR
#       )
#   )
# 
# # let´s plot the PAR values for ER again:
# 
# # filt_ER_60 <- filter(co2_cut_60_keep, type == "ER")
# # 
# # plot(x= filt_ER_60$datetime, y= filt_ER_60$PAR) # Plot the PAR vs time
# # abline(h=0, col="red")
# # 
# # unique(filt_ER_60[filt_ER_60$PAR > 60,]$fluxID) # identify the weird values 
# # range(filt_ER_60[filt_ER_60$PAR > 60,]$PAR) # and the PAR levels (no big deal)
# # unique(filt_ER_60[filt_ER_60$PAR > 60,]$datetime) # who was on the field at this time...
# # 
# # co2_cut_60_keep %>% 
# #   filter(
# #     type == "NEE"
# #   ) %>% 
# #   ggplot(aes(datetime, PAR)) +
# #   geom_point()+
# #   theme(axis.text=element_text(size=12),
# #         axis.title=element_text(size=14,face="bold"))
# # 
# # # for ER we look at the range of PAR to see if there are errors
# # filter(co2_cut_60_keep, type == "ER") %>% #faster than looking at the graph!
# #   summarise(
# #     rangePAR = range(PAR)
# #   )
# 
# # ... what should we do now??
# # 1. think about weird PAR values. what could be happening, and how to solve it? (Discuss in class)
# 
# # 2. we should also manually modify the cuts for those curve that does not look fine with the automatic cuts.
# 
# 
# 
# # calculation of fluxes ---------------------------------------------------
# 
# cflux_hogsete <- co2_cut_keep %>% 
#   flux.calc.PFTC6()
# 
# # pvalue and R2 rule ------------------------------------------------------
# p = 0.01
# R2 = 0.7
# 
# cflux_hogsete_clean <- cflux_hogsete %>%
#   mutate(
#     flux = case_when(
#       "p.value" > p  & adj.r.squared < R2 & type == "NEE" ~ 0,
#       "p.value" > p  & adj.r.squared < R2 & type == "ER" ~ NA_real_,
#       "p.value" <= p & "adj.r.squared" < R2 ~ NA_real_,
#       "p.value" > p & "adj.r.squared" >= R2 ~ flux,
#       "p.value" <= p & "adj.r.squared" >= R2 ~ flux
#       # , TRUE ~ flux
#     ))
# 
# cflux_hogsete_GPP <- GPP.PFTC6(cflux_hogsete)
# cflux_hogsete_GPP_clean <- GPP.PFTC6(cflux_hogsete_clean)
# 
# # cflux_hogsete_GPP_clean %>%
# #   filter(type == "GPP") %>%
# #   count(
# #     flux > 0
# #   )
# 
# 
# # verifying data ----------------------------------------------------------
# 
# # cflux_hogsete_GPP %>%
# #   filter(
# #     type != "NEE"
# #   ) %>%
# #   ggplot(aes(x = datetime, y = flux, color = type)) +
# #   geom_point() +
# #   # geom_text(aes(label = turfID)) +
# #   scale_x_datetime(date_breaks = "2 hours", minor_breaks = "30 min", date_labels = "%e/%m \n %H:%M")
# # 
# # cflux_hogsete_GPP_clean %>%
# #   # filter(
# #   #   type != "NEE"
# #   # ) %>%
# #   ggplot(aes(x = datetime, y = flux, color = type)) +
# #   geom_point() +
# #   # geom_text(aes(label = turfID)) +
# #   scale_x_datetime(date_breaks = "2 hours", minor_breaks = "30 min", date_labels = "%e/%m \n %H:%M")
# 
# 
# # cflux_hogsete <- GPP.PFTC6(cflux_hogsete)
# 
# cflux_hogsete_corrected <- GPP_corr.PFTC6(cflux_hogsete_GPP_clean,
#                                             start_night = "23:00:00",
#                                             end_night = "04:00:00",
#                                             strategy = "max")
# 
# cflux_hogsete_corrected %>%
#   filter(
#     type != "NEE"
#   ) %>%
#   ggplot(aes(x = time, y = flux_corrected, color = type)) +
#   geom_point() 
# # geom_text(aes(label = turfID))
# 
# write_csv(cflux_hogsete_corrected, "clean_data/PFTC6_24h-cflux_hogsete_2022.csv")
# 
