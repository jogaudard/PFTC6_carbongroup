## fluxes plots for presenting

source("code/vikesland/cleaning_vikesland.R")
source("code/metaturf.R")

# right join the metaturf     ------------------------------------------------
#(we are adding here the treatments, sites, and so on)
cflux_vikesland <- right_join(
  cflux_vikesland, metaturf)

## GPP over 24 h  -----------------------------------------------------------
# filter(type =="ER") %>% 
  # filter(flux>=0) %>% 
  
FluxPlot_vikesland <- cflux_vikesland %>% 
     filter(type != "NEE") %>%
  mutate(
    type = str_replace_all(type, c(
      "ER" = "Ecosystem Respiration",
      "GEP" = "Gross Primary Production"
    ))
    ) %>% 
     # ggplot( aes(
     #  x = datetime, y = flux, group= turfID, color = warming)) +
   ggplot( aes(
    x = datetime, y = flux, color = warming)) +
  geom_point() +
  facet_grid(type ~ ., scales = "free") +
  geom_smooth(method = "lm",
              formula = y ~ poly(x, 3),
              se = TRUE, size = 0.5, fullrange = FALSE) +

  geom_hline(
    yintercept = 0, linetype = "dashed", colour = "black") +
  
  geom_vline(
  xintercept = as.numeric(cflux_vikesland$datetime[505]),
    linetype = 2, colour = "orange", size=1) +
 geom_vline(
    xintercept = as.numeric(cflux_vikesland$datetime[81]),
    linetype = 2, colour = "blue", size=1) +
  
  scale_x_datetime(breaks = date_breaks("2 hour"), labels = date_format("%b %d - %H:%M")) +
  ggtitle("Vikesland (469 m a.s.l.)\nFluctuating Ecosystem Respiration pattern over 24 hours") +
  ylab("CO2 umol m2 h-1") +
  xlab("Time of the day (hours)") +
  theme(strip.text.y = element_text(size = 14, colour = "black"),
    axis.ticks = element_line(size=1.5), 
        axis.text.x = element_text(angle = 20, vjust = 0.8, hjust=0.8),
        axis.title = element_text(size = 16, color ="darkgrey"),
        #axis.title.x = element_blank(),
        axis.line = element_line(color = "grey"),
        axis.text = element_text(size = 12),
        legend.position = "bottom",
        legend.text = element_text(size = 12),
        legend.title = element_blank(),
       # legend.title = element_text(size = 8),
        #legend.key.width = unit(0.4, 'cm'),
        #panel.grid.major.x = element_blank(), 
        #panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.title = element_text(size=16)) +
# panel.background = element_rect(
#  fill = 'white', colour = 'grey')) +
scale_color_manual(values = c(
  "ambient" = "#1e90ff",
  "transplant" = "#ff0800"
)) 



  
  