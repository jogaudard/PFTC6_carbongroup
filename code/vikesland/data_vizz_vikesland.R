
library(ggplot2)
library(dplyr)
source("code/metaturf.R")
#library(patchwork) # To display 2 charts together
#library(hrbrthemes)

# A few constants
#neeColor <- "#69b3a2"
#erColor <- rgb(0.2, 0.6, 0.9, 1)

data = cflux_vikesland

# I am creating here a unique ID for each plot and measurement
data = data %>% 
  mutate(
    uniqueID =
      case_when(
        type == "NEE" ~ fluxID,
        type == "ER" ~ fluxID-1
  )
)

NEE_vikesland_60 <- data %>% 
   select(fluxID, PARavg,temp_soilavg, turfID, type, datetime, flux, uniqueID) %>% 
   filter(type=="NEE") %>% 
   rename(fluxID_NEE = fluxID, PARavg_NEE= PARavg, temp_soilavg_NEE = temp_soilavg, datetime_NEE = datetime)

NEE_vikesland_60

ER_vikesland_60 <- data %>% 
  select(fluxID, PARavg,temp_soilavg, turfID, type, datetime, flux, uniqueID) %>% 
  filter(type=="ER") %>% 
  rename(fluxID_ER = fluxID, PARavg_ER= PARavg, temp_soilavg_ER = temp_soilavg, datetime_ER = datetime)

ER_vikesland_60 

ER_vikesland_60 <- ER_vikesland_60 %>% 
pivot_wider(names_from = type, values_from = flux)

NEE_vikesland_60 <- NEE_vikesland_60 %>% 
  pivot_wider(names_from = type, values_from = flux)


flux_vikesland_60 <- merge(NEE_vikesland_60, ER_vikesland_60, by = c("turfID", "uniqueID"))


flux_vikesland_60 <- flux_vikesland_60 %>% 
  select("turfID", "uniqueID", "fluxID_NEE", "fluxID_ER", "datetime_NEE", "datetime_ER", "PARavg_NEE", "PARavg_ER", "temp_soilavg_NEE", "temp_soilavg_ER", "NEE", "ER")

# calculate GPP     ------------------------------------------------
flux_vikesland_60 <- flux_vikesland_60 %>% 
  mutate(flux_vikesland_60
    GPP = NEE - ER
  )

# right join the metaturf     ------------------------------------------------
#(we are adding here the treatments, sites, and so on)
flux_vikesland_60 <- right_join(
  flux_vikesland_60, metaturf)

# Check    -------------------------------------------------------------------
# plot(x = flux_virk_60$datetime_ER, y = flux_virk_60$GPP, col="red")
# points(x = flux_virk_60$datetime_ER, y = flux_virk_60$ER, col ="blue")
# points(x = flux_virk_60$datetime_ER, y = flux_virk_60$NEE, col ="blue")


# #################### HERE
# flux_virk_60 %>% 
# ggplot(aes(x = datetime_NEE, y = GPP)) +
#   geom_line(aes(color = turfID)) +
#   theme(axis.text=element_text(size=12),
#         axis.title=element_text(size=14,face="bold"))
# 
# flux_virk_60 %>% 
#   ggplot(aes(x = datetime_NEE, y = ER)) +
#   geom_line(aes(color = turfID))
