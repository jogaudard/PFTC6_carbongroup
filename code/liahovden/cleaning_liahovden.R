
#This script will be to separate c-flux data into turfIDs and clean the fluxes before we calculate them

source("code/functions.R")

library("dataDownloader")

# download raw data
# download files from OSF ---------------------------------------

get_file(node = "pk4bg",
         file = "Three-D_24h-cflux_liahovden_2022.csv",
         path = "raw_data",
         remote_path = "RawData/C-Flux")

get_file(node = "pk4bg",
         file = "PFTC6_cflux_field-record_liahovden.csv",
         path = "raw_data",
         remote_path = "RawData/C-Flux")

get_file(node = "pk4bg",
         file = "PFTC6_cflux_cutting_liahovden.csv",
         path = "raw_data",
         remote_path = "RawData/C-Flux")

# If you manage to download dataDownloader and download the data, you are good! Congrats!
# In case you did not manage to download the data manually. Call me :-)

# cleaning liahovden ------------------------------------------------------
# read the files
co2_24h_liahovden <- read_csv("raw_data/Three-D_24h-cflux_liahovden_2022.csv", na = c("#N/A"))

record_liahovden <- read_csv("raw_data/PFTC6_cflux_field-record_liahovden.csv", na = c(""))

# matching the CO2 concentration data with the turfs using the field record
# we have defined a default window length of 60 secs.


co2_fluxes_liahovden_60 <- match.flux.PFTC6(co2_24h_liahovden, record_liahovden, window_length = 60, date_format = "ymd")

# cutting liahovden ------------------------------------------------------
cutting_liahovden <- read_csv("raw_data/PFTC6_cflux_cutting_liahovden.csv", na = "", col_types = "dcc")

cutting_liahovden$start_cut <- gsub("(\\d{2})(?=\\d{2})", "\\1:", cutting_liahovden$start_cut, perl = TRUE) # to add the : in the time
cutting_liahovden$end_cut <- gsub("(\\d{2})(?=\\d{2})", "\\1:", cutting_liahovden$end_cut, perl = TRUE) # to add the : in the time

co2_cut_liahovden_60 <- co2_fluxes_liahovden_60 %>% 
  left_join(cutting_liahovden, by = "fluxID") %>% 
  mutate(
    start_cut = ymd_hms(paste(date, .$start_cut)),
    end_cut = ymd_hms(paste(date, .$end_cut))
  )

# adjusting the time window with manual cuts ------------------------------------------------------

co2_cut_liahovden_60 <- co2_cut_liahovden_60 %>%
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

# vizz liahovden -------------------------------------------------------

# visualizing 60 secs cuts in liahovden (it´s in comments, just in case you don´t want to visualize it)

theme_set(theme_grey(base_size = 5))
# 
co2_cut_liahovden_60 %>%
ggplot(aes(x = datetime, y = CO2, colour = cut)) +
geom_line(size = 0.2, aes(group = fluxID)) +
# geom_line(size = 0.2) +
scale_x_datetime(date_breaks = "1 min", minor_breaks = "10 sec", date_labels = "%e/%m \n %H:%M") +
# scale_x_date(date_labels = "%H:%M:%S") +
facet_wrap(vars(fluxID), ncol = 30, scales = "free")

ggsave("fluxes_details_liahovden.png", height = 40, width = 80, units = "cm")


# produce clean CO2 cut --------------------------------------------------------

co2_cut_60_keep <- filter(co2_cut_liahovden_60,
                          cut == "keep")  #to keep only the part we want to keep

# cleaning PAR --------------------------------------------------------------

# for ER we look at the range of PAR to see if there are errors
filter(co2_cut_60_keep, type == "ER") %>% #faster than looking at the graph!
  summarise(
    rangePAR = range(PAR)
  )

# visualize PAR levels

filt_ER_60 <- filter(co2_cut_60_keep, type == "ER") # I am just filtering to make things easier

plot(filt_ER_60$PAR) # Plot the PAR values
plot(x= filt_ER_60$datetime, y= filt_ER_60$PAR) # Plot the PAR vs time
abline(h=0, col="red")

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

filt_ER_60 <- filter(co2_cut_60_keep, type == "ER")

plot(x= filt_ER_60$datetime, y= filt_ER_60$PAR) # Plot the PAR vs time
abline(h=0, col="red")

#unique(filt_ER_60[filt_ER_60$PAR > 60,]$fluxID) # identify the weird values 
#range(filt_ER_60[filt_ER_60$PAR > 60,]$PAR) # and the PAR levels (no big deal)
#unique(filt_ER_60[filt_ER_60$PAR > 60,]$datetime) # who was on the field at this time...

co2_cut_60_keep %>% 
  filter(
    type == "NEE"
  ) %>% 
  ggplot(aes(datetime, PAR)) +
  geom_point()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

# for ER we look at the range of PAR to see if there are errors
filter(co2_cut_60_keep, type == "ER") %>% #faster than looking at the graph!
  summarise(
    rangePAR = range(PAR)
  )

# ... what should we do now??
# 1. think about weird PAR values. what could be happening, and how to solve it? (Discuss in class)

# 2. we should also manually modify the cuts for those curve that does not look fine with the automatic cuts.

# we removed fluxID 86 because it seems that the cover was not properly on

co2_cut_60_keep <- co2_cut_60_keep %>%
  filter(fluxID!=86)

# calculation of fluxes ---------------------------------------------------

cflux_liahovden <- co2_cut_60_keep %>% 
  flux.calc.PFTC6()

write_csv(cflux_liahovden, "clean_data/Three-D_24h-cflux_liahovden_2022.csv")

