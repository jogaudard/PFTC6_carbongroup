library(tidyverse)
library(lubridate)
library(hms)
library(patchwork)
library(dataDownloader)

get_file(node = "fcbw4",
         file = "PFTC6_24h_cflux_allsites_2022.csv",
         path = "clean_data",
         remote_path = "c_flux_data")

get_file(node = "fcbw4",
         file = "PFTC6_microclimate_allsites_2022.csv",
         path = "clean_data",
         remote_path = "microclimate")

fluxes <- read_csv("clean_data/PFTC6_24h_cflux_allsites_2022.csv") 

microclimate <- read_csv("clean_data/PFTC6_microclimate_allsites_2022.csv")

# Current code ----

# Set up data ----
fluxes <- read_csv("clean_data/PFTC6_24h_cflux_allsites_2022.csv")|>
  left_join(metaturf) %>%
mutate(datetime = ymd_hms(datetime),
       time = as_hms(datetime))%>%
  mutate(destSiteID = factor(destSiteID, levels = c("Lia", "Joa", "Hog", "Vik"), 
                             labels = c("Liahovden", "Joasete", "Hogsete", "Vikesland")),
         origSiteID = factor(origSiteID, levels = c("Lia", "Joa", "Hog", "Vik"), 
                             labels = c("Liahovden", "Joasete", "Hogsete", "Vikesland")))

# Graphing functions ----
plot.flux.time.site = function(orig.site, flux.type, starttime, ylim1, ylim2, title) {
  
  ggplot(fluxes %>% filter(destSiteID == orig.site) %>% filter(type == flux.type), 
         aes(y = flux, x = time, color = warming)) +
  geom_point() +
  geom_smooth(method = "loess", span = 0.3) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  geom_vline(xintercept = lubridate::hm(starttime), linetype = "dotted") +
  scale_color_manual(values = c("dodgerblue4", "firebrick4")) +
  facet_grid(type ~.) +
  ylim(ylim1, ylim2) +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text.y = element_blank())+
  labs(title = title, x = "Time") 
}

plot.par.time.site = function(dest.site) {
  ggplot(fluxes %>% filter(destSiteID == dest.site) %>% filter(type == "GPP"), 
         aes(y = PARavg, x = time)) +
  geom_smooth(method = "loess", span = 1/3, color = "goldenrod1", se = FALSE) +
  stat_smooth(
    geom = 'area', method = 'loess', span = 1/3,
    fill = "goldenrod1", alpha = 0.5) + 
  geom_point(color = "goldenrod2") +
  scale_color_manual(values = c("dodgerblue4", "firebrick4")) +
  theme_bw() +
  scale_y_log10() +
  theme(strip.background = element_blank(),
        strip.text.y = element_blank()
  )+
  labs(title = "Light (PAR)", x = "") 
}

# Make the plots ----
## All sites together ----

# Check ylims
min(fluxes$flux[fluxes$type == "ER" & fluxes$warming == "A"], na.rm = TRUE)
max(fluxes$flux[fluxes$type == "ER" & fluxes$warming == "A"], na.rm = TRUE)

min(fluxes$flux[fluxes$type == "GPP" & fluxes$warming == "A"], na.rm = TRUE)
max(fluxes$flux[fluxes$type == "GPP" & fluxes$warming == "A"], na.rm = TRUE)

cflux.plot.er =
ggplot(fluxes %>% filter(type == "ER") %>% filter(warming == "A"), 
       aes(y = flux, x = time, color = origSiteID)) +
  geom_point() +
  geom_smooth(method = "loess", span = 0.3) +
  scale_color_manual(values = c("#005a32", "#238443", "#41ab5d", "#78c679")) +
  ylim(0, 115) +
  theme_bw() +
  labs(x = "Time", title = "Ecosystem respiration (ER)") 

cflux.plot.gpp =
ggplot(fluxes %>% filter(type == "GPP") %>% filter(warming == "A"), 
       aes(y = flux, x = time, color = origSiteID)) +
  geom_point() +
  geom_smooth(method = "loess", span = 0.3) +
  scale_color_manual(values = c("#005a32", "#238443", "#41ab5d", "#78c679")) +
  ylim(-100, 60) +
  theme_bw() +
  labs(x = "Time", title = "Gross primary productivity (GPP)") 

png("visualizations/flux_PAR_allsites.png", res = 300, units = "in", width = 10, height = 8)
cflux.plot.gpp / cflux.plot.er
dev.off()


## Warming vs ambient ----

cflux.plot.warm.er = 
ggplot(fluxes %>% filter(origSiteID  %in% c("Liahovden", "Joasete")) %>% filter(type == "ER"), 
       aes(y = flux, x = time, color = warming, shape = origSiteID)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", span = 0.3, aes(linetype = origSiteID)) +
  scale_color_manual(values = c("dodgerblue4", "firebrick4")) +
  scale_linetype_manual(values = c("solid", "longdash")) +
  facet_grid(type ~., scales = "free") +
  ylim(0, 115) +
  theme_bw() +
  labs(x = "Time", title = "Ecosystem respiration (ER)") 

cflux.plot.warm.gpp = 
  ggplot(fluxes %>% filter(origSiteID  %in% c("Liahovden", "Joasete")) %>% filter(type == "GPP"), 
         aes(y = flux, x = time, color = warming, shape = origSiteID)) +  
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", span = 0.3, aes(linetype = origSiteID)) +
  scale_color_manual(values = c("dodgerblue4", "firebrick4")) +
  scale_linetype_manual(values = c("solid", "longdash")) +
  facet_grid(type ~., scales = "free") +
  ylim(-100, 60) +
  theme_bw() +
  labs(x = "Time", title = "Gross primary productivity (GPP)") 

png("visualizations/flux_PAR_warming.png", res = 300, units = "in", width = 10, height = 8)
cflux.plot.warm.gpp / cflux.plot.warm.er
dev.off()

## Vikesland ----
vik.plot.er = plot.flux.time.site("Vikesland", "ER", "21:10", -70, 80, "Ecosystem respiration (ER)")
vik.plot.gpp = plot.flux.time.site("Vikesland", "GPP", "21:10", -70, 80, "Gross primary productivity (GPP)")
vik.plot.par = plot.par.time.site("Vikesland")

vik.plot.gpp + vik.plot.par + vik.plot.er +
  plot_layout(ncol = 1, heights = c(2, 1, 2)) +
  plot_annotation(title = "Vikesland")

png("visualizations/flux_PAR_Vik.png", res = 300, units = "in", width = 10, height = 10)
dev.off()

## Hogsete ----
min(cflux_hogsete$flux_corrected[cflux_hogsete$type=="ER"], na.rm = TRUE)
max(cflux_hogsete$flux_corrected[cflux_hogsete$type=="ER"], na.rm = TRUE)

min(cflux_hogsete$flux_corrected[cflux_hogsete$type=="GPP"], na.rm = TRUE)
max(cflux_hogsete$flux_corrected[cflux_hogsete$type=="GPP"], na.rm = TRUE)

hog.plot.er = plot.flux.time.site("Hogsete", "ER", "22:30", 0, 90, "Ecosystem respiration (ER)")
hog.plot.gpp = plot.flux.time.site("Hogsete", "GPP", "22:30", -90, 0, "Gross primary productivity (GPP)")
hog.plot.par = plot.par.time.site("Hogsete")

hog.plot.gpp + hog.plot.par + hog.plot.er +
  plot_layout(ncol = 1, heights = c(2, 1, 2)) +
  plot_annotation(title = "Hogsete")

png("visualizations/flux_PAR_Hog.png", res = 300, units = "in", width = 10, height = 10)
dev.off()

## Joasete ----
joa.plot.er = plot.flux.time.site("Joasete", "ER", "07:00", -70, 80, "Ecosystem respiration (ER)") +
  geom_vline(xintercept = lubridate::hm("04:00"), linetype = "dotted") 
joa.plot.gpp = plot.flux.time.site("Joasete", "GPP", "07:00", -70, 80, "Gross primary productivity (GPP)") +
  geom_vline(xintercept = lubridate::hm("04:00"), linetype = "dotted") 
joa.plot.par = plot.par.time.site("Joasete")

joa.plot.gpp + joa.plot.par + joa.plot.er +
  plot_layout(ncol = 1, heights = c(2, 1, 2)) +
  plot_annotation(title = "Joasete")

png("visualizations/flux_PAR_Joa.png", res = 300, units = "in", width = 10, height = 10)
dev.off()

## Liahovden ----
lia.plot.er = plot.flux.time.site("Liahovden", "ER", "05:20", -70, 80, "Ecosystem respiration (ER)")
lia.plot.gpp = plot.flux.time.site("Liahovden", "GPP", "05:20", -70, 80, "Gross primary productivity (GPP)")
lia.plot.par = plot.par.time.site("Liahovden")

lia.plot.gpp + lia.plot.par + lia.plot.er +
  plot_layout(ncol = 1, heights = c(2, 1, 2)) +
  plot_annotation(title = "Liahovden")

png("visualizations/flux_PAR_Lia.png", res = 300, units = "in", width = 10, height = 10)
dev.off()

# ARCHIVED CODE ----
# Merge data from all sites ----

# The metadata is still messy because something went wrong with the site
meta_seedclim <- tibble(
  turfID = c("TTC 101", "TTC 110", "TTC 115", "TTC 146", "TTC 140", "TTC 141"),
  destSiteID = c("Hog", "Hog", "Hog", "Vik", "Vik", "Vik"),
  origSiteID = c("Hog", "Hog", "Hog", "Vik", "Vik", "Vik"),
  warming = "A"
) %>%
  bind_rows(read_csv("raw_data/Three-D_metaturfID.csv")) %>%
  mutate(turfID = as.character(turfID)) 

# Join all the data together ----
cflux_all = cflux_vikesland %>%
  bind_rows(cflux_joasete) %>%
  bind_rows(cflux_hogsete) %>%
  bind_rows(cflux_liahovden) %>%
  rename(flux_old = flux, flux = flux_corrected) %>%
  left_join(meta_seedclim) %>%
  mutate(datetime = ymd_hms(datetime),
         time = as_hms(datetime))%>%
  mutate(destSiteID = factor(destSiteID, levels = c("Lia", "Joa", "Hog", "Vik"), 
                             labels = c("Liahovden", "Joasete", "Hogsete", "Vikesland")),
         origSiteID = factor(origSiteID, levels = c("Lia", "Joa", "Hog", "Vik"), 
                             labels = c("Liahovden", "Joasete", "Hogsete", "Vikesland")))


colnames(cflux_all)

