
# This script will be to separate c-flux data into turfIDs and clean the fluxes before we calculate them

source("code/functions.R")

library("dataDownloader")

library("tidyverse")
library("scales")


# download raw data
# download files from OSF ---------------------------------------

get_file(node = "pk4bg",
         file = "Three-D_24h-cflux_vikesland_2022.csv",
         path = "raw_data",
         remote_path = "RawData/C-Flux")

get_file(node = "pk4bg",
         file = "PFTC6_cflux_field-record_vikesland.csv",
         path = "raw_data",
         remote_path = "RawData/C-Flux")

get_file(node = "pk4bg",
         file = "PFTC6_cflux_cutting_vikesland.csv",
         path = "raw_data",
         remote_path = "RawData/C-Flux")

# If you manage to download dataDownloader and download the data, you are good! Congrats!
# In case you did not manage to download the data manually. Call me :-)

# cleaning Vikesland ------------------------------------------------------
# read the files
co2_24h_vikesland <- read_csv("raw_data/Three-D_24h-cflux_vikesland_2022.csv", na = c("#N/A"))
  
record_vikesland <- read_csv("raw_data/PFTC6_cflux_field-record_vikesland.csv", na = c(""))

# matching the CO2 concentration data with the turfs using the field record
# we have defined a default window length of 60 secs.

co2_fluxes_vikesland_60 <- match.flux.PFTC6(co2_24h_vikesland, record_vikesland, window_length = 120)

# cutting Vikesland ------------------------------------------------------
cutting_vikesland <- read_csv("raw_data/PFTC6_cflux_cutting_vikesland.csv", na = "", col_types = "dcc")

co2_cut_vikesland_60 <- co2_fluxes_vikesland_60 %>% 
  left_join(cutting_vikesland, by = "fluxID") %>% 
  mutate(
    start_cut = ymd_hms(paste(date, .$start_cut)),
    end_cut = ymd_hms(paste(date, .$end_cut))
  )

# adjusting the time window with manual cuts ------------------------------------------------------

co2_cut_vikesland_60 <- co2_cut_vikesland_60 %>%
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

# visualizing 60 secs cuts in Vikesland (it´s in comments, just in case you don´t want to visualize it)

# theme_set(theme_grey(base_size = 5))
# 
co2_cut_vikesland_60 %>%
  ggplot(aes(x = datetime, y = CO2, colour = cut)) +
  geom_line(size = 0.2, aes(group = fluxID)) +
  # geom_line(size = 0.2) +
  scale_x_datetime(date_breaks = "1 min", minor_breaks = "10 sec", date_labels = "%e/%m \n %H:%M") +
  # scale_x_date(date_labels = "%H:%M:%S") +
  facet_wrap(vars(fluxID), ncol = 30, scales = "free")

ggsave("fluxes_details_vikesland.png", height = 40, width = 80, units = "cm")

co2_cut_vikesland_60_flux233 <- co2_cut_vikesland_60 %>% 
  filter(fluxID == 233) %>% 
  ggplot(aes(x = datetime, y = CO2, colour = cut)) +
  geom_line(size = 0.2, aes(group = fluxID)) +
  # geom_line(size = 0.2) +
  scale_x_datetime(date_breaks = "1 min", minor_breaks = "10 sec", date_labels = "%e/%m \n %H:%M")

co2_cut_vikesland_60_flux105 <- co2_cut_vikesland_60 %>% 
  filter(fluxID == 105) %>% 
  ggplot(aes(x = datetime, y = CO2, colour = cut)) +
  geom_line(size = 0.2, aes(group = fluxID)) +
  # geom_line(size = 0.2) +
  scale_x_datetime(date_breaks = "1 min", minor_breaks = "10 sec", date_labels = "%e/%m \n %H:%M")


# produce clean CO2 cut --------------------------------------------------------

co2_cut_60_keep <- filter(co2_cut_vikesland_60,
                  cut == "keep")  #to keep only the part we want to keep

# cleaning PAR ------------------------------------------

# for ER we look at the range of PAR to see if there are errors
filter(co2_cut_60_keep, type == "ER") %>% #faster than looking at the graph!
  summarise(
    rangePAR = range(PAR)
  )

# for NEE we look at the range of PAR to see if there are errors

filter(co2_cut_60_keep, type == "NEE") %>% #faster than looking at the graph!
  summarise(
    rangePAR = range(PAR)
  )

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

PAR_wrong_duringER_plot <- co2_cut_60_keep %>% 
  filter(type=="ER") %>% 
  ggplot(aes(x = datetime,  y = PAR)) +
  geom_point(alpha = 1/10, size = 2) +
  geom_hline(
    yintercept = 0, linetype = "dashed", colour = "red") +
  scale_x_datetime(breaks = breaks_width("2 hour"), labels = date_format("%b %d - %H:%M")) +
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
  

# now we are replacing negative PAR values in type=ER by zero values.

co2_cut_60_keep <- co2_cut_60_keep %>% 
  mutate(
    PAR =
  case_when(
    type=="ER" & PAR <= 0 ~ 0, 
    TRUE~PAR
    )
  )

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


#unique(filt_ER_60[filt_ER_60$PAR > 60,]$fluxID) # identify the weird values 
#range(filt_ER_60[filt_ER_60$PAR > 60,]$PAR) # and the PAR levels (no big deal)
#unique(filt_ER_60[filt_ER_60$PAR > 60,]$datetime) # who was on the field at this time...

#  NEE ---------------------------------

filt_NEE_60 <- filter(co2_cut_60_keep, type == "NEE") # I am just filtering to make things easier

# quick plot with base R
plot(filt_NEE_60$PAR) # Plot the PAR values
plot(x= filt_NEE_60$datetime, y= filt_NEE_60$PAR,
     xlab = "Time of the day (hours)", 
     ylab = "Photosynthetically active radiation (PAR)",
     col = alpha("blue", 0.1), pch=16,
) # Plot the PAR vs time
abline(h = 0, col="blue")

# same plot with ggplot2
PAR_wrong_duringNEE_plot <- co2_cut_60_keep %>% 
  filter(type=="NEE") %>% 
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

# Visualize individual PAR measures-----------------------------------------

theme_set(theme_grey(base_size = 5))
 
filt_NEE_60 %>%
  ggplot(aes(x = datetime, y = PAR, colour = cut)) +
  geom_point(size = 0.2, aes(group = fluxID)) +
  # geom_line(size = 0.2) +
  scale_x_datetime(date_breaks = "1 min", minor_breaks = "10 sec", date_labels = "%e/%m \n %H:%M") +
  # scale_x_date(date_labels = "%H:%M:%S") +
  facet_wrap(vars(fluxID), ncol = 30, scales = "free")

# Replace negative and odd PAR values in type = NEE by NA values ------------

co2_cut_60_keep <- co2_cut_60_keep %>% 
  mutate(
    PAR =
      case_when(
          fluxID == "195"
          ~ NA_real_, 
        TRUE~PAR
      )
  ) %>% 
  mutate(
    PAR =
      case_when(
        fluxID == "143"
        ~ NA_real_, 
        TRUE~PAR
      )
  ) %>% 
  mutate(
    PAR =
      case_when(
        fluxID == "153"
        ~ NA_real_, 
        TRUE~PAR
      )
  ) %>% 
  mutate(
    PAR =
      case_when(
        fluxID == "155"
        ~ NA_real_, 
        TRUE~PAR
      )
  ) %>% 
  mutate(
    PAR =
      case_when(
        fluxID == "177"
        ~ NA_real_, 
        TRUE~PAR
      )
  ) %>% 
  mutate(
    PAR =
      case_when(
        fluxID == "179"
        ~ NA_real_, 
        TRUE~PAR
      )
  ) %>% 
  mutate(
    PAR =
      case_when(
        fluxID == "161" & PAR < 50
        ~ NA_real_, 
        TRUE~PAR
      )
  )

# quick plot with base R ---------------------------------------
filt_NEE_60 <- filter(co2_cut_60_keep, type == "NEE") # I am just filtering to make things easier
plot(filt_NEE_60$PAR) # Plot the PAR values
plot(x= filt_NEE_60$datetime, y= filt_NEE_60$PAR,
     xlab = "Time of the day (hours)", 
     ylab = "Photosynthetically active radiation (PAR)",
     col = alpha("blue", 0.1), pch=16,
) # Plot the PAR vs time

abline(h = 0, col="blue")
############################################################################3

# same plot with ggplot2 -----------------------------------
PAR_right_duringNEE_plot <- co2_cut_60_keep %>% 
  filter(type=="NEE") %>% 
  ggplot(aes(x = datetime,  y = PAR)) +
  geom_point(alpha = 1/10, size = 2) +
  geom_hline(
    yintercept = 0, linetype = "dashed", colour = "red") +
  scale_x_datetime(breaks = date_breaks("2 hour"), labels = date_format("%b %d - %H:%M")) +
  ggtitle("Vikesland (469 m a.s.l.)\nPAR values over 24 h") +
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


# Discussion with students in the class  ----------------------------------

# ... what should we do now??
# 1. think about weird PAR values. what could be happening, and how to solve it? (Discuss in class)

# 2. we should also manually modify the cuts for those curve that does not look fine with the automatic cuts.


# calculation of fluxes ---------------------------------------------------

cflux_vikesland <- co2_cut_60_keep %>% 
  flux.calc.PFTC6()


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

cflux_vikesland <- GPP.PFTC6(cflux_vikesland)


# write_csv(cflux_vikesland, "clean_data/Three-D_24h-cflux_vikesland_2022.csv")


