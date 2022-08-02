
library(ggplot2)
library(dplyr)
library(patchwork) # To display 2 charts together
library(hrbrthemes)

# A few constants
neeColor <- "#69b3a2"
erColor <- rgb(0.2, 0.6, 0.9, 1)

data = flux.virkesland.90

# I am creating here a unique ID for each plot and measurement
data = data %>% 
  mutate(
    uniqueID =
      case_when(
        type == "NEE" ~ fluxID,
        type == "ER" ~ fluxID-1
  )
)

NEE_virk_90 <- data %>% 
   select(fluxID, PARavg,temp_soilavg, turfID, type, datetime, flux, uniqueID) %>% 
   filter(type=="NEE") %>% 
   rename(fluxID_NEE = fluxID, PARavg_NEE= PARavg, temp_soilavg_NEE = temp_soilavg, datetime_NEE = datetime)

NEE_virk_90

ER_virk_90 <- data %>% 
  select(fluxID, PARavg,temp_soilavg, turfID, type, datetime, flux, uniqueID) %>% 
  filter(type=="ER") %>% 
  rename(fluxID_ER = fluxID, PARavg_ER= PARavg, temp_soilavg_ER = temp_soilavg, datetime_ER = datetime)

ER_virk_90 

ER_virk_90 <- ER_virk_90 %>% 
pivot_wider(names_from = type, values_from = flux)

NEE_virk_90 <- NEE_virk_90 %>% 
  pivot_wider(names_from = type, values_from = flux)


flux_virk_90 <- merge(NEE_virk_90, ER_virk_90, by = c("turfID", "uniqueID"))


flux_virk_90 <- flux_virk_90 %>% 
  select("turfID", "uniqueID", "fluxID_NEE", "fluxID_ER", "datetime_NEE", "datetime_ER", "PARavg_NEE", "PARavg_ER", "temp_soilavg_NEE", "temp_soilavg_ER", "NEE", "ER")


flux_virk_90 <- flux_virk_90 %>% 
  mutate(
    GPP = NEE - ER
  )

plot(x = flux_virk_90$datetime_ER, y = flux_virk_90$GPP, col="red")
points(x = flux_virk_90$datetime_ER, y = flux_virk_90$ER, col ="red")
points(x = flux_virk_90$datetime_ER, y = flux_virk_90$NEE, col ="blue")

flux_virk_90 %>% 
ggplot(aes(x = datetime_NEE, y = GPP)) +
  geom_line(aes(color = turfID))

flux_virk_90 %>% 
  ggplot(aes(x = datetime_NEE, y = ER)) +
  geom_line(aes(color = turfID))
