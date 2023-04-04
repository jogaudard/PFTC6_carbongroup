# Import data from OSF ----
library(dataDownloader)
get_file(node = "fcbw4",
         file = "PFTC6_leaf_area_2022.csv",
         path = "clean_data",
         remote_path = "raw_data/trait_raw_data")

get_file(node = "fcbw4",
         file = "PFTC6_clean_leaf_traits_2022.csv",
         path = "clean_data",
         remote_path = "trait_data")

get_file(node = "pk4bg",
         file = "THREE-D_Cover_2019_2020.csv",
         path = "clean_data",
         remote_path = "Vegetation")

# I don't know how to import data from an sqlite file in R
# These data are from https://osf.io/6tu4p 

#Read in data ----
## Traits ----
leaf.area = read.csv("clean_data/PFTC6_leaf_area_2022.csv") %>%
  select(ID, leaf_area)

# traits = read.csv("clean_data/PFTC6_clean_leaf_traits_2022.csv") %>%
#   separate(plotID, into = c("origin", "trt", "destination"), sep = " ", remove = FALSE) %>%
#   left_join(leaf.area) %>%
#   mutate(sla = leaf_area/wet_mass_g)

traits = read.csv("clean_data/PFTC6_clean_leaf_traits_2022.csv") %>%
  mutate(sla = leaf_area_total_cm2/wet_mass_total_g)

table(traits$trt)

## Flux -----
cflux.traits = cflux_all %>%
  separate(turfID, into = c("origin", "trt", "destination"), sep = " ", remove = FALSE) %>%
  mutate(datetime = ymd_hms(datetime),
         time = hms::as_hms(datetime))%>%
  mutate(since_midnight = hour(time) * 60 + minute(time)) %>% 
  filter(since_midnight >= (9*60)+50 & since_midnight < (14*60)+10) %>%
  select(turfID, type, temp_soil, PARavg, datetime, time, flux, since_midnight)

table(cflux.traits$trt)

## Cover -----
# Bring in seedclim turf data
cover.seedclim = read.csv("raw_data/seedclim_turf_community.csv") %>%
  left_join(read.csv("raw_data/seedclim_turfs.csv")) %>%
  select(turfID, destinationPlotID, originPlotID, species, cover, year) %>%
  rename(destPlotID = destinationPlotID, origPlotID = originPlotID) %>%
  mutate(trt = case_when(
    origPlotID == destPlotID ~ "A",
    TRUE ~ "W"
  )) %>%
  filter(year == 2019)
  
cover.3d = read.csv("clean_data/THREE-D_Cover_2019_2020.csv") %>%
  select(turfID, year, species, cover, destPlotID, origPlotID) %>%
  mutate(destPlotID = as.character(destPlotID), origPlotID = as.character(origPlotID)) %>%
  mutate(trt = case_when(
    origPlotID == destPlotID ~ "A",
    TRUE ~ "W"
  )) %>%
  filter(year == 2019)

cover = cover.seedclim %>%
  bind_rows(cover.3d)

# cover = read.csv("clean_data/THREE-D_Cover_2019_2020.csv") %>%
#   separate(turfID, into = c("origin", "trt", "destination"), sep = " ", remove = FALSE) %>%
#   filter(year == 2020) %>%
#   select(trt, destination, species, cover)

# Calculate community weighted means ----
# https://rpubs.com/CPEL/cwm 
traits.cover = traits %>%
  select(turfID, species, plant_height, sla) %>%
  # rename(species = taxon) %>%
  pivot_longer(cols = c(plant_height, sla), values_to = "value", names_to = "trait") %>%
  #Add in cover data
  left_join(cover) %>%
  # NOTE that this removes much of the data 
  # drop_na(cover) %>%
  #Calculate weighted means
  group_by(trt, destination, trait) %>%
  summarize(wtd.mean = weighted.mean(value, cover, na.rm = TRUE)) %>%
  pivot_wider(names_from = trait, values_from = wtd.mean)

# Plant height as a variable has gone missing. Trying code without it.
traits.cover = traits %>%
  select(turfID, species, sla) %>%
  # rename(species = taxon) %>%
  pivot_longer(cols = c(sla), values_to = "value", names_to = "trait") %>%
  #Add in cover data
  left_join(cover) %>%
  # NOTE that this removes much of the data 
  drop_na(cover) %>%
  #Calculate weighted means
  group_by(turfID, trt, trait) %>%
  summarize(wtd.mean = weighted.mean(value, cover, na.rm = TRUE)) %>%
  pivot_wider(names_from = trait, values_from = wtd.mean)
  
# Add CWMs to flux ----
flux.cwm = cflux.traits %>%
  left_join(traits.cover) %>%
  mutate(type = factor(type, levels = c("GPP", "NEE", "ER"))) %>%
  filter(warming == "A") %>%
  pivot_longer(cols = c(plant_height, sla), values_to = "CWM", names_to = "trait") %>%
  drop_na(CWM)

# Without height
flux.cwm = cflux.traits %>%
  left_join(traits.cover) %>%
  mutate(type = factor(type, levels = c("GPP", "NEE", "ER"))) %>%
  filter(trt == "A") %>%
  pivot_longer(cols = c(sla), values_to = "CWM", names_to = "trait") %>%
  drop_na(CWM)
  
table(flux.cwm$turfID)

# First visualizations ----
library(ggpmisc)

ggplot(flux.cwm, aes(y = flux, x = CWM)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(grp.label, ..eq.label.., ..rr.label.., ..p.value.label.., sep = "~~~")),
               #label.y = 60, size = 3, col = "black",
               size = 3, parse = TRUE)+ 
  facet_grid(type ~ trait, scales = "free") +
  theme_bw()
