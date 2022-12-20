
# This script will be to separate c-flux data into turfIDs and clean the fluxes before we calculate them

source("code/functions.R")

library("dataDownloader")

library("tidyverse")
library("scales")

# download raw data
# download files from OSF ---------------------------------------

get_file(node = "fcbw4",
         file = "PFTC6_CO2_vikesland_2022.csv",
         path = "raw_data",
         remote_path = "raw_data/c_flux_raw_data")

get_file(node = "fcbw4",
         file = "PFTC6_cflux_field-record_vikesland.csv",
         path = "raw_data",
         remote_path = "raw_data/c_flux_raw_data")

# get_file(node = "fcbw4",
         # file = "PFTC6_cflux_cutting_vikesland.csv",
         # path = "raw_data",
         # remote_path = "raw_data/c_flux_raw_data")

# If you manage to download dataDownloader and download the data, you are good! Congrats!
# In case you did not manage to download the data manually. Call me :-)

# cleaning Vikesland ------------------------------------------------------
# read the files
co2_24h_vikesland <- read_csv("raw_data/Three-D_24h-cflux_vikesland_2022.csv", na = c("#N/A"))

record_vikesland <- read_csv("raw_data/PFTC6_cflux_field-record_vikesland.csv", na = c(""))

# matching the CO2 concentration data with the turfs using the field record
# we have defined a default window length of 60 secs.

co2_fluxes_vikesland <- match.flux.PFTC6(co2_24h_vikesland, record_vikesland, startcrop = 10, window_length = 100, measurement_length = 180)

# cutting Vikesland ------------------------------------------------------
# cutting_vikesland <- read_csv("raw_data/PFTC6_cflux_cutting_vikesland.csv", na = "", col_types = "dcc")
# cutting_vikesland <- tibble( #bypass manual cuts
#   fluxID = c(1:5),
#   start_cut = NA,
#   end_cut = NA
# )
# 
# co2_cut_vikesland <- co2_fluxes_vikesland %>%
#   left_join(cutting_vikesland, by = "fluxID") %>%
#   mutate(
#     start_cut = ymd_hms(paste(date, .$start_cut)),
#     end_cut = ymd_hms(paste(date, .$end_cut))
#   )


# adjusting the time window with manual cuts ------------------------------------------------------

co2_fluxes_vikesland <- co2_fluxes_vikesland %>%
  mutate(
    # start_window = case_when(
    #   is.na(start_cut) == FALSE ~ start_cut,
    #   TRUE ~ start_window
    # ),
    # end_window = case_when(
    #   is.na(end_cut) == FALSE ~ end_cut,
    #   TRUE ~ end_window
    # ),
    cut = case_when(
      datetime <= start_window | datetime >= end_window ~ "cut",
      # fluxID ==  & datetime %in%  ~ "cut",
      fluxID == 113 ~ "cut", #starts at 900ppm
      fluxID == 185 ~ "cut", #starts at 900ppm
      fluxID == 118 ~ "cut", #starts at 600ppm
      fluxID == 133 ~ "cut", #starts at 600ppm
      fluxID == 134 ~ "cut", #starts at 600ppm
      TRUE ~ "keep"
    ),
    cut = as_factor(cut)
  )


# automatic cutting -------------------------------------------------------

# In Zhao et al 2018, the idea is to identify the first stationnary point (derivative is 0) and cut the measurement there.

# Here is the equation we want to use:
# C = Cm + a*(t-tz) + (Cz - Cm)*exp(-b*(t-tz))
# where C is CO2 concentration
# Cm is peak (or dip!?) at beginning of measurement
# t is time since start of measurement
# tz is time at which C = Cz (theoretically tz=0?)
# Cz is the intercept of linear regression of first 15s of measurement

# finding Cz and tz

Cz_window <- 15
Cm_window <- 100



Cm_df <- co2_fluxes_vikesland %>% 
  # filter(
  #   cut == "keep"
  # ) %>% 
  group_by(fluxID) %>% 
  distinct(CO2, .keep_all = TRUE) %>% 
  mutate(
    time = difftime(datetime[1:length(datetime)],datetime[1] , units = "secs"),
    time = as.double(time)
  ) %>% 
  # filter(
  #   time < Cm_window
  # ) %>% 
  mutate(
    Cmax = max(CO2),
    Cmin = min(CO2),
    # tmax = case_when(
    #         CO2 == Cmax ~ time,
    #         CO2 =! Cmax ~ NA_real_
    #       ),
    tmax = time[CO2 == Cmax],
    tmin = time[CO2 == Cmin]
  ) %>% 
  select(fluxID, Cmax, Cmin, tmax, tmin) %>% 
  ungroup() %>% 
  distinct(Cmax, Cmin, .keep_all = TRUE)

Cm_slope <- co2_fluxes_vikesland %>% 
  # filter(
  #   cut == "keep"
  # ) %>% 
  group_by(fluxID) %>% 
  mutate(
    time = difftime(datetime[1:length(datetime)],datetime[1] , units = "secs"),
    time = as.double(time)
  ) %>% 
  # filter(
  #   time < Cm_window
  # ) %>% 
  do({model = lm(CO2 ~ time, data=.)    # create your model
  data.frame(tidy(model),              # get coefficient info
             glance(model))}) %>%          # get model info
  filter(term == "time") %>% 
  rename(slope = estimate) %>% 
  select(fluxID, slope) %>% 
  ungroup()

Cm_df <- left_join(Cm_df, Cm_slope) %>% 
  mutate(
    Cm = case_when(
      slope < 0 ~ Cmin, 
      slope > 0 ~ Cmax 
    ),
    tm = case_when(
      slope < 0 ~ tmin,
      slope > 0 ~ tmax
    )
  ) %>% 
  select(fluxID, Cm, tm) %>% 
  ungroup()

# tz_df <- co2_fluxes_vikesland %>% 
#   left_join(Cm_df) %>% 
#   # filter(
#   #   cut == "keep"
#   # ) %>% 
#   group_by(fluxID) %>% 
#   distinct(CO2, .keep_all = TRUE) %>% # in case there are identical CO2 values
#   mutate(
#     time = difftime(datetime[1:length(datetime)],datetime[1] , units = "secs"),
#     time = as.double(time)
#   ) %>% 
#   # select(fluxID, time, CO2, Cm) %>%
#   mutate(
#     tz = case_when(
#       CO2 == Cm ~ time,
#       CO2 =! Cm ~ NA_real_
#     )
#   ) %>% 
#   fill(tz) %>% 
#   ungroup() %>% 
#   select(fluxID, tz) %>% 
#   drop_na(tz) %>% 
#   unique()

# Cz_df <- co2_fluxes_vikesland %>% 
#   left_join(tz_df) %>% 
#   group_by(fluxID) %>% 
#   mutate(
#     time = difftime(datetime[1:length(datetime)],datetime[1] , units = "secs"),
#     time = as.double(time)
#   ) %>%
#   filter(
#     time >= tz
#     & time <= tz + Cz_window
#   ) %>% 
#   do({model = lm(CO2 ~ time, data=.)    # create your model
#   data.frame(tidy(model),              # get coefficient info
#              glance(model))}) %>%          # get model info
#   filter(term == "(Intercept)") %>% 
#   rename(Cz = estimate) %>% 
#   select(fluxID, Cz) %>% 
#   ungroup()

Cz_df <- co2_fluxes_vikesland %>%
  # filter(
  #   cut == "keep"
  # ) %>%
  group_by(fluxID) %>%
  mutate(
    time = difftime(datetime[1:length(datetime)],datetime[1] , units = "secs"),
    time = as.double(time)
  ) %>%
  select(fluxID, time, CO2) %>%
  filter(
    time < Cz_window
  ) %>%
  do({model = lm(CO2 ~ time, data=.)    # create your model
  data.frame(tidy(model),              # get coefficient info
             glance(model))}) %>%          # get model info
  filter(term == "(Intercept)") %>%
  rename(Cz = estimate) %>%
  select(fluxID, Cz) %>%
  ungroup()

# Cz_df <- co2_fluxes_vikesland %>% 
#   left_join(Cm_df) %>%
#   group_by(fluxID) %>%
#   mutate(
#     time = difftime(datetime[1:length(datetime)],datetime[1] , units = "secs"),
#     time = as.double(time),
#     time = time - tm
#   ) %>%
#   filter(
#     time >= 0
#     & time <= Cz_window
#   ) %>% 
#   # filter(
#   #   time >= tm
#   #   & time <= tm + Cz_window
#   # ) %>%
#   do({model = lm(CO2 ~ time, data=.)    # create your model
#   data.frame(tidy(model),              # get coefficient info
#              glance(model))}) %>%          # get model info
#   filter(term == "(Intercept)") %>%
#   rename(Cz = estimate) %>%
#   select(fluxID, Cz) %>%
#   ungroup()

# tz_df <- co2_fluxes_vikesland %>% 
#   left_join(Cz_df) %>% 
#   # filter(
#   #   cut == "keep"
#   # ) %>% 
#   group_by(fluxID) %>% 
#   distinct(CO2, .keep_all = TRUE) %>% # in case there are identical CO2 values
#   mutate(
#     time = difftime(datetime[1:length(datetime)],datetime[1] , units = "secs"),
#     time = as.double(time)
#   ) %>% 
#   # select(fluxID, time, CO2, Cm) %>%
#   mutate(
#     tz = case_when(
#       CO2 == Cz ~ time,
#       CO2 =! Cz ~ NA_real_
#     )
#   ) %>% 
#   # fill(tz) %>% 
#   ungroup() %>% 
#   select(fluxID, tz) %>% 
#   drop_na(tz) %>% 
#   unique()

# try with fluxID 111 -----------------------------------------------------


test_df <- co2_fluxes_vikesland %>% 
  filter(
    fluxID == 157
    # & cut == "keep"
    ) %>%
  left_join(Cm_df) %>% 
  left_join(Cz_df) %>% 
  # left_join(tz_df) %>% 
  mutate(
    time = difftime(datetime[1:length(datetime)],datetime[1] , units = "secs"),
    time = as.numeric(time)
    # time = time - tm
    # time = time - tz,
    # cut = case_when(
    #   time >= tz ~ "keep",
    #   time < tz ~ "cut"
    # )
  ) %>% 
  # filter(
  #   time >= 0
  #   & time <= 120
  # ) %>%
  # filter(
  #   time >= 25
  # ) %>%
  select(time, CO2, Cz) 
  # filter(cut == "keep")

# nlc <- nls.control(maxiter = 1000)
# model <- nls(CO2 ~ Cm+a*(time-tz)+(Cz-Cm)*exp(-b*(time-tz)), test_df, start = list(a = 1, b= -1, tz=50), control = nlc)
# model <- nls(CO2 ~ CM + a * time - a *t + (Cz - Cm)*exp(b*t)*exp(-b*time), test_df)
# model2 <- nls(CO2 ~ Cm + (Cz - Cm)*exp(-b*(time-tz)), test_df, start = list(b=10))

# I need to understand how to guess the start!

# model3 <- nlme(CO2 ~ Cm+a*(time-tz)+(Cz-Cm)*exp(-b*(time-tz)), test_df)

# model4 <- optimise(CO2 ~ Cm+a*(time-tz)+(Cz-Cm)*exp(-b*(time-tz))

# f <- function(data, a, tz, b) (Cm+a*(time-tz)+(Cz-Cm)*exp(-b*(time-tz)))

# Cmin <- optimise(f, c(50, 200), tol = 0.0001)

# myfn <- function(data, par) {
#   with(data, sqrt((1/length(time)) * sum((Cm+par[1]*(time-par[2])+(Cz-Cm)*exp(-par[3]*(time-par[2]))-CO2)^2)))
# }

myfn <- function(data, par) {
  with(data, sqrt((1/length(time)) * sum((par[1]+par[2]*(time-par[4])+(Cz-par[1])*exp(-par[3]*(time-par[4]))-CO2)^2)))
}

# myfn <- function(data, par) {
#   with(data, sqrt((1/length(time)) * sum((Cz+par[1]*(time-par[2])+(Cm-Cz)*exp(-par[3]*(time-par[2]))-CO2)^2)))
# }

results <- optim(par = c(500, -1,0.001, 60), fn = myfn, data = test_df)

# optimise(myfn, c(-100, 200), data = test_df)
  

# optimize()

# deriv(Cm+a*(time-tz)+(Cz-Cm)*exp(-b*(time-tz), time)

test_df <- test_df %>% 
  mutate(
    # fit = predict(model)
    # fit = Cm+results$par[1]*(time-results$par[2])+(Cz-Cm)*exp(-results$par[3]*(time-results$par[2]))
    # fit = Cz+results$par[1]*(time-results$par[2])+(Cm-Cz)*exp(-results$par[3]*(time-results$par[2]))
    fit = results$par[1]+results$par[2]*(time-results$par[4])+(Cz-results$par[1])*exp(-results$par[3]*(time-results$par[4]))
    # fit = Cm + results$par[1]*(time-results$par[2])
    # fit = 561.8 + 0.15*(time - 10) + (562.2-561.8)*exp(-0*(time- 10))
    # fit = 518 + 0.15 * (time -10)
    # fit = 535 + (562-535)*exp(-0.03*(time- 80))
    # fit = 530 + 0.15 * (time - 120) + (535 - 530) * exp(-0.035 * (time -120))
    # fit = 545 + 0.15 * (time - 40) + (Cz - 545) * exp(-0.09 * (time - 40))
    
  )

ggplot(test_df, aes(time, CO2)) +
  geom_point() +
  geom_line(aes(time, fit))
  # ylim(NA, 600)

results$par

summary(model)

# fun <- function(tz, a, b, Cm, Cz, time) Cm+a*(time-tz)+(Cz-Cm)*exp(-b*(time-tz)) 
# 
# opt <- optimise(fun, c(min(CO2):max(CO2)), Cm=test_df$Cm, Cz=test_df$Cz, time=test_df$time)

D(expr= Cm + (Cz - Cm)*exp(-b*(time - tz)), name= time)




coefficients <- left_join(co2_fluxes_vikesland, Cz_df) %>% 
  left_join(Cm_df) %>% 
  filter(
    cut == "keep"
  ) %>%
  # select(fluxID, datetime, CO2, Cz) %>% 
  mutate(
    time = difftime(datetime[1:length(datetime)],datetime[1] , units = "secs")
  ) %>% 
  select(fluxID, time, CO2, Cz, Cm) %>%
  do({model = CO2 ~ Cm + a*(time-tz) + (Cz - Cm)*exp(-b*(t-tz))   # create your model
  data.frame(tidy(model),              # get coefficient info
             glance(model))}) %>%          # get model info
  filter(term == "(Intercept)") %>% 
  rename(Cz = estimate) %>% 

# vizz Vikesland -------------------------------------------------------
# 
# # visualizing 60 secs cuts in Vikesland (it´s in comments, just in case you don´t want to visualize it)
# 
# theme_set(theme_grey(base_size = 5))
# co2_cut_vikesland_60 %>%
#    ggplot(aes(x = datetime, y = CO2, colour = cut)) +
#    geom_line(size = 0.2, aes(group = fluxID)) +
#    # geom_line(size = 0.2) +
#    scale_x_datetime(date_breaks = "1 min", minor_breaks = "10 sec", date_labels = "%e/%m \n %H:%M") +
#    # scale_x_date(date_labels = "%H:%M:%S") +
#    facet_wrap(vars(fluxID), ncol = 30, scales = "free")
# 
# ggsave("fluxes_details_vikesland.png", height = 40, width = 80, units = "cm")
# 

theme_set(theme_grey(base_size = 5))
co2_fluxes_vikesland %>%
  mutate(
    fluxID = as.numeric(fluxID)
  ) %>% 
  filter(
    fluxID %in% c(140:160)
  ) %>%
   ggplot(aes(x = datetime, y = CO2, colour = cut)) +
   geom_line(size = 0.2, aes(group = fluxID)) +
   # geom_line(size = 0.2) +
   scale_x_datetime(date_breaks = "1 min", minor_breaks = "10 sec", date_labels = "%e/%m \n %H:%M") +
   # scale_x_date(date_labels = "%H:%M:%S") +
   facet_wrap(vars(fluxID), ncol = 3, scales = "free")


# produce clean CO2 cut --------------------------------------------------------

co2_cut_keep <- filter(co2_cut_vikesland,
                          cut == "keep")  #to keep only the part we want to keep

# cleaning PAR ------------------------------------------

# for ER we look at the range of PAR to see if there are errors
# filter(co2_cut_60_keep, type == "ER") %>% #faster than looking at the graph!
#   summarise(
#     rangePAR = range(PAR)
#   )

# for NEE we look at the range of PAR to see if there are errors

# filter(co2_cut_60_keep, type == "NEE") %>% #faster than looking at the graph!
#   summarise(
#     rangePAR = range(PAR)
#   )

# visualize PAR level ---------------------------------
# ER ---------------------------------


filt_ER_60 <- filter(co2_cut_60_keep, type == "ER") # I am just filtering to make things easier

# quick base R plot of PAR vs time
plot(x= filt_ER_60$datetime, y= filt_ER_60$PAR,
     xlab = "Time of the day (hours)", 
     ylab = "Photosynthetically active radiation (PAR)",
     col = alpha("black", 0.1), pch=20, main= "Vikesland (469 m a.s.l.)\nPAR during ER measures")

abline(h=0, col="red")


## same plot on ggplot2

# PAR_wrong_duringER_plot <- co2_cut_60_keep %>% 
#   filter(type=="ER") %>% 
#   ggplot(aes(x = datetime,  y = PAR)) +
#   geom_point(alpha = 1/10, size = 2) +
#   geom_hline(
#     yintercept = 0, linetype = "dashed", colour = "red") +
#   scale_x_datetime(breaks = date_breaks("2 hour"), labels = date_format("%b %d - %H:%M")) +
#   ggtitle("Vikesland (469 m a.s.l.)\nEcosystem Respiration PAR values") +
#   theme(axis.ticks = element_line(size=1.5), 
#         axis.text.x = element_text(angle = 20, vjust = 0.8, hjust=0.8),
#         axis.title = element_text(size = 14, color ="darkgrey"),
#         axis.title.x = element_blank(),
#         axis.line = element_line(color = "grey"),
#         axis.text = element_text(size = 12),
#         # legend.position = "none",
#         legend.text = element_text(size = 8),
#         legend.title = element_text(size = 8),
#         #legend.key.width= unit(0.4, 'cm'),
#         #panel.grid.major.x = element_blank(), 
#         #panel.grid.major.y = element_blank(),
#         panel.grid.minor.x = element_blank(),
#         panel.grid.minor.y = element_blank(),
#         plot.title = element_text(size=16))
# panel.background = element_rect(
#  fill = 'white', colour = 'grey'))
PAR_wrong_duringER_plot

# now we are replacing negative PAR values in type=ER by zero values.

# co2_cut_60_keep <- co2_cut_60_keep %>% 
#   mutate(
#     PAR =
#       case_when(
#         type=="ER" & PAR <= 0 ~ 0, 
#         TRUE~PAR
#       )
#   )

# let´s plot the PAR values for ER again:
# quick plot with base R

filt_ER_60 <- filter(co2_cut_60_keep, type == "ER")

plot(x= filt_ER_60$datetime, y= filt_ER_60$PAR) # Plot the PAR vs time
abline(h=0, col="red")

# same plot with ggplot2

PAR_right_duringER_plot <- co2_cut_60_keep %>% 
  filter(type=="ER") %>% 
  ggplot(aes(x = datetime,  y = PAR)) +
  geom_point(alpha = 1/10, size = 2) +
  geom_hline(
    yintercept = 0, linetype = "dashed", colour = "red") +
  scale_x_datetime(breaks = date_breaks("2 hour"), labels = date_format("%b %d - %H:%M")) +
  ggtitle("Vikesland (469 m a.s.l.)\nEcosystem Respiration PAR values") +
  theme(axis.ticks = element_line(size=1.5), 
        axis.text.x = element_text(angle = 20, vjust = 0.8, hjust=0.8),
        axis.title = element_text(size = 14, color ="darkgrey"),
        axis.title.x = element_blank(),
        axis.line = element_line(color = "grey"),
        axis.text = element_text(size = 12),
        # legend.position = "none",
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        #legend.key.width= unit(0.4, 'cm'),
        #panel.grid.major.x = element_blank(), 
        #panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.title = element_text(size=16))
# panel.background = element_rect(
#  fill = 'white', colour = 'grey'))

PAR_right_duringER_plot


#unique(filt_ER_60[filt_ER_60$PAR > 60,]$fluxID) # identify the weird values 
#range(filt_ER_60[filt_ER_60$PAR > 60,]$PAR) # and the PAR levels (no big deal)
#unique(filt_ER_60[filt_ER_60$PAR > 60,]$datetime) # who was on the field at this time...

#  NEE ---------------------------------

# filt_NEE_60 <- filter(co2_cut_60_keep, type == "NEE") # I am just filtering to make things easier
# 
# # quick plot with base R
# plot(filt_NEE_60$PAR) # Plot the PAR values
# plot(x= filt_NEE_60$datetime, y= filt_NEE_60$PAR,
#      xlab = "Time of the day (hours)", 
#      ylab = "Photosynthetically active radiation (PAR)",
#      col = alpha("blue", 0.1), pch=16,
# ) # Plot the PAR vs time
# abline(h = 0, col="blue")
# 
# # same plot with ggplot2
# PAR_wrong_duringNEE_plot <- co2_cut_60_keep %>% 
#   filter(type=="NEE") %>% 
#   ggplot(aes(x = datetime,  y = PAR)) +
#   geom_point(alpha = 1/10, size = 2) +
#   geom_hline(
#     yintercept = 0, linetype = "dashed", colour = "red") +
#   scale_x_datetime(breaks = date_breaks("2 hour"), labels = date_format("%b %d - %H:%M")) +
#   ggtitle("Vikesland (469 m a.s.l.)\nEcosystem Respiration PAR values") +
#   theme(axis.ticks = element_line(size=1.5), 
#         axis.text.x = element_text(angle = 20, vjust = 0.8, hjust=0.8),
#         axis.title = element_text(size = 14, color ="darkgrey"),
#         axis.title.x = element_blank(),
#         axis.line = element_line(color = "grey"),
#         axis.text = element_text(size = 12),
#         # legend.position = "none",
#         legend.text = element_text(size = 8),
#         legend.title = element_text(size = 8),
#         #legend.key.width= unit(0.4, 'cm'),
#         #panel.grid.major.x = element_blank(), 
#         #panel.grid.major.y = element_blank(),
#         panel.grid.minor.x = element_blank(),
#         panel.grid.minor.y = element_blank(),
#         plot.title = element_text(size=16))
# # panel.background = element_rect(
# #  fill = 'white', colour = 'grey'))
# 
# # Visualize individual PAR measures-----------------------------------------
# 
# theme_set(theme_grey(base_size = 5))
# 
# filt_NEE_60 %>%
#   ggplot(aes(x = datetime, y = PAR, colour = cut)) +
#   geom_point(size = 0.2, aes(group = fluxID)) +
#   # geom_line(size = 0.2) +
#   scale_x_datetime(date_breaks = "1 min", minor_breaks = "10 sec", date_labels = "%e/%m \n %H:%M") +
#   # scale_x_date(date_labels = "%H:%M:%S") +
#   facet_wrap(vars(fluxID), ncol = 30, scales = "free")
# 
# # Replace negative and odd PAR values in type = NEE by NA values ------------
# 
filter(co2_cut_keep, type == "ER") %>% #faster than looking at the graph!
  summarise(
    rangePAR = range(PAR)
  )

filter(co2_cut_keep, type == "NEE") %>% #faster than looking at the graph!
  summarise(
    rangePAR = range(PAR, na.rm = TRUE)
  )

co2_cut_keep %>% 
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

co2_cut_keep %>% 
  mutate(
    fluxID = as.numeric(fluxID)
  ) %>% 
  filter(
    fluxID %in% c(13)
  ) %>% 
  mutate(
    datetime = ymd_hms(datetime),
    time = hms::as_hms(datetime)
  ) %>% 
  ggplot(aes(x = time, y = PAR)) +
  geom_point() +
  facet_wrap(vars(fluxID), scales = "free")

#negative PAR for ER should be 0
co2_cut_keep <- co2_cut_keep %>% 
  mutate(
    PAR = case_when(
      type == "ER" & PAR < 0 ~ 0,
      TRUE ~ PAR
    )
  )

co2_cut_keep <- co2_cut_keep %>%
  mutate(
    fluxID = as.numeric(fluxID),
    PAR =
      case_when(
        # fluxID == 195
        # | fluxID == 143
        # | fluxID == 153
        # | fluxID == 155
        # | fluxID == 177
        # | fluxID == 179
        # | fluxID == 31
        # | fluxID ==
        fluxID %in% c(195, 143, 153, 155, 177, 179, 31, 118, 148)
        | (fluxID %in% c(29, 57, 27, 35, 41, 67, 15, 25, 157, 17, 11, 37, 13) & PAR < 0)
        | (fluxID == 161 & PAR < 50)
        ~ NA_real_,
        TRUE ~ PAR
      )
  )

# # quick plot with base R ---------------------------------------
# filt_NEE_60 <- filter(co2_cut_60_keep, type == "NEE") # I am just filtering to make things easier
# plot(filt_NEE_60$PAR) # Plot the PAR values
# plot(x= filt_NEE_60$datetime, y= filt_NEE_60$PAR,
#      xlab = "Time of the day (hours)", 
#      ylab = "Photosynthetically active radiation (PAR)",
#      col = alpha("blue", 0.1), pch=16,
# ) # Plot the PAR vs time
# 
# abline(h = 0, col="blue")
# ############################################################################3
# 
# # same plot with ggplot2 -----------------------------------
# PAR_right_duringNEE_plot <- co2_cut_60_keep %>% 
#   filter(type=="NEE") %>% 
#   ggplot(aes(x = datetime,  y = PAR)) +
#   geom_point(alpha = 1/10, size = 2) +
#   geom_hline(
#     yintercept = 0, linetype = "dashed", colour = "red") +
#   scale_x_datetime(breaks = date_breaks("2 hour"), labels = date_format("%b %d - %H:%M")) +
#   ggtitle("Vikesland (469 m a.s.l.)\nPAR values over 24 h") +
#   theme(axis.ticks = element_line(size=1.5), 
#         axis.text.x = element_text(angle = 20, vjust = 0.8, hjust=0.8),
#         axis.title = element_text(size = 14, color ="darkgrey"),
#         axis.title.x = element_blank(),
#         axis.line = element_line(color = "grey"),
#         axis.text = element_text(size = 12),
#         # legend.position = "none",
#         legend.text = element_text(size = 8),
#         legend.title = element_text(size = 8),
#         #legend.key.width= unit(0.4, 'cm'),
#         #panel.grid.major.x = element_blank(), 
#         #panel.grid.major.y = element_blank(),
#         panel.grid.minor.x = element_blank(),
#         panel.grid.minor.y = element_blank(),
#         plot.title = element_text(size=16))
# # panel.background = element_rect(
# #  fill = 'white', colour = 'grey'))


# Discussion with students in the class  ----------------------------------

# ... what should we do now??
# 1. think about weird PAR values. what could be happening, and how to solve it? (Discuss in class)

# 2. we should also manually modify the cuts for those curve that does not look fine with the automatic cuts.


# calculation of fluxes ---------------------------------------------------

cflux_vikesland <- co2_cut_keep %>% 
  flux.calc.PFTC6()

# pvalue and R2 rule ------------------------------------------------------
p = 0.01
R2 = 0.7

cflux_vikesland_clean <- cflux_vikesland %>% 
  mutate(
    flux = case_when(
      "p.value" > p  & adj.r.squared < R2 & type == "NEE" ~ 0,
      "p.value" > p  & adj.r.squared < R2 & type == "ER" ~ NA_real_,
      "p.value" <= p & "adj.r.squared" < R2 ~ NA_real_,
      "p.value" > p & "adj.r.squared" >= R2 ~ flux,
      "p.value" <= p & "adj.r.squared" >= R2 ~ flux
      # , TRUE ~ flux
    ))

cflux_vikesland_GPP <- GPP.PFTC6(cflux_vikesland)
cflux_vikesland_GPP_clean <- GPP.PFTC6(cflux_vikesland_clean)

# cflux_vikesland_GPP_clean %>% 
#   filter(type == "GPP") %>% 
#   count(
#     flux > 0
#   )


# verifying data ----------------------------------------------------------

# cflux_vikesland_GPP %>% 
#   filter(
#     type != "NEE"
#   ) %>% 
#   ggplot(aes(x = datetime, y = flux, color = type)) +
#   geom_point() +
#   # geom_text(aes(label = turfID)) +
#   scale_x_datetime(date_breaks = "2 hours", minor_breaks = "30 min", date_labels = "%e/%m \n %H:%M")

# cflux_vikesland_GPP_clean %>% 
#   # filter(
#   #   type != "NEE"
#   # ) %>% 
#   ggplot(aes(x = datetime, y = flux, color = type)) +
#   geom_point() +
#   # geom_text(aes(label = turfID)) +
#   scale_x_datetime(date_breaks = "2 hours", minor_breaks = "30 min", date_labels = "%e/%m \n %H:%M")


# calculating GPP ---------------------------------------------------------

# cflux_vikesland_GPP <- cflux_vikesland %>%
#   mutate(
#     pairID = case_when(
#       type == "NEE" ~ fluxID,
#       type == "ER" ~ fluxID-1
#     ),   # problem with datetime, it is different between ER and NEE. Let's use datetime NEE
#     # datetime = case_when(
#     #   type == "NEE" ~ datetime,
#     #   type == "ER" ~ NA_real_
#     # ),
#     turfID = as_factor(turfID),
#     type = as_factor(type)
#   ) %>%
#   select(!c(fluxID)) %>%
#   # pivot_wider(names_from = type, values_from = PARavg, names_prefix = "PARavg_") %>%
#   # select(!c(PAR_corrected_flux)) %>%
#   # select(campaign, turfID, date, type, corrected_flux) %>%
#   pivot_wider(names_from = type, values_from = c(flux, temp_soilavg, datetime, PARavg)) %>%
# 
#   # pivot_wider(names_from = type, values_from = c(flux, temp_soilavg)) %>%
#   rename(
#     ER = flux_ER,
#     NEE = flux_NEE
#   ) %>%
#   mutate(
#     GEP = NEE - ER
#   ) %>%
#   pivot_longer(c(ER, NEE, GEP), names_to = "type", values_to = "flux") %>%
#   mutate(
#     temp_soil = case_when(
#       type == "ER" ~ temp_soilavg_ER,
#       type == "NEE" ~ temp_soilavg_NEE,
#       type == "GEP" ~ rowMeans(select(., c(temp_soilavg_NEE, temp_soilavg_ER)), na.rm = TRUE)
#     ),
#     PARavg = case_when(
#       type == "ER" ~ PARavg_ER,
#       type == "NEE" ~ PARavg_NEE,
#       type == "GEP" ~ PARavg_NEE
#       ),
#     datetime = case_when(
#       type == "ER" ~ datetime_ER,
#       type == "NEE" ~ datetime_NEE,
#       type == "GEP" ~ datetime_NEE
#     )
#   ) %>%
#   select(!c(temp_soilavg_ER, temp_soilavg_NEE, PARavg_ER, PARavg_NEE, datetime_ER, datetime_NEE))


# cflux_vikesland <- GPP.PFTC6(cflux_vikesland)
# 
# # remove negative ER values (no sense)
# cflux_vikesland <- cflux_vikesland[!(cflux_vikesland$type=="ER" & cflux_vikesland$flux <0),]
# 
# # remove negative ER values (no sense)
# cflux_vikesland <- cflux_vikesland[!(cflux_vikesland$type=="GEP" & cflux_vikesland$flux < -200),]
# 
# cflux_vikesland <- cflux_vikesland[!(cflux_vikesland$type=="GEP" & cflux_vikesland$flux > 100),]

cflux_vikesland_corrected <- GPP_corr.PFTC6(cflux_vikesland_GPP_clean,
                                          start_night = "22:50:00",
                                          end_night = "04:00:00",
                                          strategy = "max")

cflux_vikesland_corrected %>%
  filter(
    type != "NEE"
  ) %>%
  ggplot(aes(x = time, y = flux_corrected, color = type)) +
  geom_point()
  geom_point(size = 0.01) +
  geom_text(aes(label = turfID))

write_csv(cflux_vikesland_corrected, "clean_data/PFTC6_24h-cflux_vikesland_2022.csv")


