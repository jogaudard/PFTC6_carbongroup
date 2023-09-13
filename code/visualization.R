my_packages <- c("dataDownloader",
                 "tidyverse",
                 "lubridate",
                 "ggpubr",
                 "viridis",
                 "patchwork"
)

lapply(my_packages, library, character.only = TRUE) 

get_file(node = "fcbw4",
         file = "PFTC6_24h_cflux_allsites_2022.csv",
         path = "clean_data",
         remote_path = "c_flux_data")

get_file(node = "fcbw4",
         file = "PFTC6_microclimate_allsites_2022.csv", #this version of the file has big holes, trying to figure out with Hilary what happened
         path = "clean_data",
         remote_path = "microclimate")

fluxes <- read_csv("clean_data/PFTC6_24h_cflux_allsites_2022.csv")

microclimate <- read_csv("clean_data/PFTC6_microclimate_allsites_2022.csv") %>% 
  mutate(
    site = str_to_lower(site),
    site = as_factor(site)
    )

# need to cut fluxes so we match the time window of the fluxes

fluxes_startstop <- fluxes %>% 
  group_by(site) %>% 
  mutate(
    start = min(datetime),
    stop = max(datetime)
  ) %>% 
  select(site, start, stop) %>% 
  unique()

microclimate <- microclimate %>% 
  # mutate(
  #   # datetime = ymd_hms(datetime + 1),
  #   datetime = as.POSIXct(datetime)
  # ) %>% 
  left_join(fluxes_startstop, by = "site") %>% 
  filter(
    datetime <= stop
    & datetime >= start
    & !(datetime > ymd_hms("2022-07-29T03:10:00") & datetime < ymd_hms("2022-07-30T03:10:00"))
  )
  
# we need to do some funny stuff for joasete because some of the fluxes were measured the day after following equipment failure
  
# microclimate_temp <- microclimate %>% 
#   # mutate(
#   #   datetime = datetime +1,
#   #   datetime2 = ymd_hms(datetime)
#   # )
#   filter(
#       # site == "joasete"
#         !(datetime > ymd_hms("2022-07-29T03:10:00") & datetime < ymd_hms("2022-07-30T03:10:00"))
#   )

# # reprex for Richard about midnight issue
# tibble(
#   datetime = c("2023-09-12 23:50:00", "2022-07-28 00:00:01", "2023-09-12 00:00:01", "2022-07-29 00:00:01"),
#   a = c(1:4)
# ) %>% 
#   mutate(
#     datetime = as.POSIXct(datetime)
#     # datetime2 = ymd_hms(datetime)
#   ) %>% 
#   filter(
#     datetime %in% c(ymd_hms("2022-07-28 03:13:09"):ymd_hms("2022-07-30 03:29:10"))
#   )



  # filter(
  #   
  #     (site == "Vikesland" &
  #      # datetime %in% c(ymd_hms("2022-07-23T21:45:15"):ymd_hms("2022-07-24T22:04:00")))
  #   datetime > ymd_hms("2022-07-23T21:45:15") & datetime < ymd_hms("2022-07-24T22:04:00"))
  #   |
  #     (site == "Hogsete"
  #      & datetime > ymd_hms(""))
  # )


# arranging the data ------------------------------------------------------

# first we need to arrange the data in a weirdly mixed long format
data_long <- full_join(microclimate, fluxes, by = c("datetime", "value" = "flux_corrected", "site", "turfID")) %>% 
  


# fluxes_long <- fluxes %>% 
  mutate(
    PAR = case_when(
      type == "GPP" ~ PARavg,
      type == "ER" ~ NA_real_
    ),
    type = case_when(
      is.na(type) ~ sensor,
      TRUE ~ type
    ),
    site = str_to_lower(site)
  ) %>%
  # filter(
  #   # is.na(type) |
  #   type != "NEE"
  # ) %>% 
  select(type, PAR, datetime, site, value, turfID) %>% 
  rowid_to_column("rowID") %>% 
  # mutate(
  #   data = case_when(
  #     type == "ER" ~ value,
  #     type == "GPP" ~ flux_corrected,
  #     type == "PAR" ~
  #   )
  # )
  # pivot_wider(names_from = "type", values_from = "flux_corrected") %>% 
  pivot_wider(names_from = "type", values_from = "value") %>% 
  pivot_longer(cols = c(PAR, GPP, ER, NEE, air_temperature, soil_temperature, ground_temperature, soil_moisture)) %>% 
  drop_na(value) %>% 
  mutate(
    name = as_factor(name),
    time = hms::as_hms(datetime),
    name = factor(name, levels = c("air_temperature", "ground_temperature", "soil_temperature", "soil_moisture", "PAR", "ER", "NEE", "GPP")), # we need to make sure everything is in the same order
    site = factor(site, levels = c("liahovden", "joasete", "hogsete", "vikesland"))
  )

# liahovden ---------------------------------------------------------------


# cumulative graph --------------------------------------------------------

# cumul_fluxes <- fluxes %>% 
#   pivot_wider(names_from = "type", values_from = "flux_corrected") %>% 
#   group_by(site) %>% 
#   arrange(time) %>% 
#   mutate(
#     cumul_flux_corr_ER = replace(ER, !is.na(ER), cumsum(na.omit(ER))),
#     cumul_flux_corr_NEE = replace(NEE, !is.na(NEE), cumsum(na.omit(NEE))),
#     cumul_flux_corr_GPP = replace(GPP, !is.na(GPP), cumsum(na.omit(GPP)))
#   ) %>% 
#   select(!c(ER, NEE, GPP)) %>% 
#   pivot_longer(cols = c(cumul_flux_corr_ER, cumul_flux_corr_NEE, cumul_flux_corr_GPP), values_to = "cumul_flux_corr", names_to = "type") %>% 
#   mutate(
#     type = str_replace_all(
#       type,
#       c(
#         "cumul_flux_corr_ER" = "ER",
#         "cumul_flux_corr_NEE" = "NEE",
#         "cumul_flux_corr_GPP" = "GPP"
#       )
#     )
#   ) %>% 
#   drop_na(cumul_flux_corr)
# 
# cumul_fluxes %>% 
#   ggplot(aes(x = time, y = cumul_flux_corr)) +
#   geom_line() +
#   geom_point() +
#   facet_grid(type ~ site, scales = "free")

# diurnal -----------------------------------------------------------------



data_long %>% 
  # filter(
  #   site == "vikesland"
  # ) %>%
  ggplot(aes(time, value, color=site)) +
  geom_point(size=0.05) +
  geom_smooth() +
  facet_grid(name~., scales = "free")

# problem: we want the microclimate data only during the fluxes

# flux_graph <- fluxes %>%
#   filter(
#   site == "liahovden",
#   type != "NEE"
# ) %>%
#   ggplot(aes(x=datetime)) +
#   geom_point(aes(y=flux)) +
#   # geom_point(aes(y=PARavg)) +
#   facet_grid(type~., scales = "free")
# 
# par_graph <- fluxes %>%
#   filter(
#     site == "liahovden",
#     type == "NEE"
#   ) %>%
#   ggplot(aes(datetime, PARavg)) +
#   geom_point()
# 
# 
# 
# lia_figure <- ggarrange(flux_graph, par_graph, ncol = 1)
# lia_figure

# microclimate density ----------------------------------------------------

microclimate %>% 
  ggplot(aes(x=value, fill=site)) +
  geom_density(alpha=0.6, linewidth = 0.8) +
  scale_fill_viridis(discrete=T) +
  facet_wrap(~sensor,labeller = label_parsed, scales="free") +
  labs(
    y="Density",
    x= "Microclimate value",
    fill = "Site"
  ) +
  guides(fill = guide_legend(byrow = TRUE)) +
  theme_bw() +
  theme(text = element_text(size=14),
        strip.text = element_text(colour = "black"),
        legend.spacing.y = unit(0.1,"cm"))



# diurnal and density together --------------------------------------------

# font_size <- 7
# 
# density_microclimate <- data_long %>% 
#   filter(
#     name %in% c("air_temperature", "ground_temperature", "soil_moisture", "soil_temperature", "PAR")
#   ) %>% 
#   ggplot(aes(x=value, fill=site)) +
#   geom_density(alpha=0.6, linewidth = 0.8) +
#   scale_fill_viridis(discrete=T) +
#   scale_y_continuous(position = "right") +
#   facet_wrap(name~.,labeller = label_parsed, scales="free", ncol = 1) +
#   labs(
#     y="Density",
#     x= "Microclimate value"
#     # fill = "Site"
#   ) +
#   # guides(fill = guide_legend(byrow = TRUE)) +
#   # guides(fill = "none") +
#   theme(legend.position="none",
#         strip.text.x = element_blank(),
#         text=element_text(size=font_size)
#         # axis.title.x = element_blank(),
#         # axis.title.y = element_blank()
#         # axis.text.x=element_blank(),
#         # axis.ticks.x=element_blank(),
#         # axis.text.y=element_blank(),
#         # axis.ticks.y=element_blank()
#         )
#   # theme_bw() 
#   # theme(text = element_text(size=14),
#   #       strip.text = element_text(colour = "black"),
#   #       legend.spacing.y = unit(0.1,"cm"))
# density_microclimate
# 
# diurnal_fluxes <- data_long %>% 
#   filter(
#     # name != "NEE"
#     name %in% c("ER", "GPP", "NEE")
#   ) %>%
#   # arrange()
#   ggplot(aes(time, value, color=site)) +
#   geom_point(size=0.05) +
#   geom_smooth() +
#   facet_grid(name~., scales = "free") +
#   scale_color_viridis(discrete=T) +
#   # guides(fill = guide_legend(byrow = TRUE)) +
#   theme_bw() +
#   theme(legend.position="none") +
#   labs(
#     y="Fluxes",
#     x= "Time"
#     # fill = "Site"
#   )
# diurnal_fluxes
# 
# 
# diurnal_microclimate <- data_long %>% 
#   filter(
#     # name != "NEE"
#     name %in% c("air_temperature", "ground_temperature", "soil_moisture", "soil_temperature", "PAR")
#   ) %>%
#   # arrange()
#   ggplot(aes(time, value, color=site)) +
#   geom_point(size=0.05) +
#   geom_smooth() +
#   facet_grid(name~.,
#              scales = "free",
#              labeller = labeller(name = c(
#                air_temperature = "Air T°",
#                ground_temperature = "Ground T°",
#                soil_temperature = "Soil T°",
#                soil_moisture = "Soil moisture",
#                PAR = "PAR"
#                ))
#              ) +
#   scale_color_viridis(discrete=T) +
#   # guides(fill = guide_legend(byrow = TRUE)) +
#   theme_bw() +
#   labs(
#     y="Microclimate value"
#     # x= "Value"
#     # fill = "Site"
#   ) +
#   theme(legend.position="none",
#         axis.title.x = element_blank(),
#         axis.text.x=element_blank(),
#         axis.ticks.x=element_blank()
#         )
# diurnal_microclimate
# 
# fluxes_cumul <- data_long %>% 
#   filter(
#     name %in% c("ER", "GPP", "NEE")
#   ) %>% 
#   # pivot_wider(names_from = "name", values_from = "value") %>%
#   group_by(site, name, turfID) %>%
#   summarise(
#     cumul_flux = sum(value)
#   ) %>% 
#   ggplot(aes(y = cumul_flux, x = site, fill = site, color = site)) +
#   geom_boxplot(alpha = 0.5, outlier.shape = NA) +
#   geom_jitter() +
#   scale_fill_viridis(discrete=T, labels = c(
#     hogsete = "Hogsete",
#     joasete = "Joasete",
#     liahovden = "Liahovden",
#     vikesland = "Vikesland"
#   )) +
#   scale_color_viridis(discrete=T, labels = c(
#     hogsete = "Hogsete",
#     joasete = "Joasete",
#     liahovden = "Liahovden",
#     vikesland = "Vikesland"
#   )) +
#   scale_y_continuous(position = "right") +
#   # scale_fill_manual(labels = c(
#   #   hogsete = "Hogsete"
#   # )) +
#   labs(
#     y="Cumulative fluxes",
#     # x= "Value"
#     fill = "Site",
#     color = "Site"
#   ) +
#   facet_wrap(~name, scales = "free", ncol = 1) +
#   # labs(x = "") +
#   theme_bw() +
#   theme(
#         # legend.position = "none",
#         strip.text.x = element_blank(),
#         axis.title.x = element_blank(),
#         axis.text.x=element_blank(),
#         axis.ticks.x=element_blank()
#         # axis.text.y=element_blank(),
#         # axis.ticks.y=element_blank()
#         )
#   # theme(legend.position = "none", 
#   #       axis.text.x = element_text(angle = 45,vjust = 1, hjust=1))
# fluxes_cumul
#   # 
#   # ggplot(cum_fluxes, aes(y = total.flux, x = site, fill = site, color = site)) +
#   # geom_boxplot(alpha = 0.5, outlier.shape = NA) +
#   # geom_jitter() +
#   # scale_fill_viridis(discrete=T)+
#   # scale_color_viridis(discrete=T)+
#   # facet_wrap(~type, scales = "free") +
#   # labs(x = "") +
#   # theme_bw() +
#   # theme(legend.position = "none", 
#   #       axis.text.x = element_text(angle = 45,vjust = 1, hjust=1))
# # library(patchwork)
# # full_figure <- diurnal | (density_microclimate / fluxes_cumul) +
# # full_figure <- (density_microclimate / fluxes_cumul) | diurnal +
# # full_figure <- (diurnal_microclimate / diurnal_fluxes) | (density_microclimate / fluxes_cumul) +
# full_figure <- diurnal_microclimate + density_microclimate + diurnal_fluxes + fluxes_cumul +
#   plot_layout(guides = "collect",
#               ncol = 2,
#               widths = c(3, 1),
#               heights = c(5, 3)
#               )
# 
# full_figure

# Aud's idea to "save time" (haha)
# edit: it was actually fast and it is super practical
plots_making <- function(data_long, font_size)
{
  density_microclimate <- data_long %>% 
    filter(
      name %in% c("air_temperature", "ground_temperature", "soil_moisture", "soil_temperature", "PAR")
    ) %>% 
    ggplot(aes(x=value, fill=site)) +
    geom_density(alpha=0.6, linewidth = 0.8) +
    scale_fill_viridis(discrete=T) +
    scale_y_continuous(position = "right") +
    facet_wrap(name~.,labeller = label_parsed, scales="free", ncol = 1) +
    labs(
      y="Density",
      x= "Microclimate value"
      # fill = "Site"
    ) +
    theme_bw() +
    # guides(fill = guide_legend(byrow = TRUE)) +
    # guides(fill = "none") +
    theme(legend.position="none",
          strip.text.x = element_blank(),
          text=element_text(size=font_size)
          # axis.title.x = element_blank(),
          # axis.title.y = element_blank()
          # axis.text.x=element_blank(),
          # axis.ticks.x=element_blank(),
          # axis.text.y=element_blank(),
          # axis.ticks.y=element_blank()
    )
  # theme(text = element_text(size=14),
  #       strip.text = element_text(colour = "black"),
  #       legend.spacing.y = unit(0.1,"cm"))
  # density_microclimate
  
  diurnal_fluxes <- data_long %>% 
    filter(
      # name != "NEE"
      name %in% c("ER", "GPP", "NEE")
    ) %>%
    # arrange()
    ggplot(aes(time, value, color=site)) +
    geom_point(size=0.05) +
    geom_smooth(method = "loess", span = 0.3) +
    facet_grid(name~., scales = "free") +
    scale_color_viridis(discrete=T) +
    # guides(fill = guide_legend(byrow = TRUE)) +
    theme_bw() +
    theme(legend.position="none",
          text=element_text(size=font_size)) +
    labs(
      y="Fluxes",
      x= "Time"
      # fill = "Site"
    )
  # diurnal_fluxes
  
  
  diurnal_microclimate <- data_long %>% 
    filter(
      # name != "NEE"
      name %in% c("air_temperature", "ground_temperature", "soil_moisture", "soil_temperature", "PAR")
    ) %>%
    # arrange()
    ggplot(aes(time, value, color=site)) +
    geom_point(size=0.05) +
    geom_smooth(method = "loess", span = 0.3) +
    facet_grid(name~.,
               scales = "free",
               labeller = labeller(name = c(
                 air_temperature = "Air T°",
                 ground_temperature = "Ground T°",
                 soil_temperature = "Soil T°",
                 soil_moisture = "Soil moisture",
                 PAR = "PAR"
               ))
    ) +
    scale_color_viridis(discrete=T) +
    # guides(fill = guide_legend(byrow = TRUE)) +
    theme_bw() +
    labs(
      y="Microclimate value"
      # x= "Value"
      # fill = "Site"
    ) +
    theme(legend.position="none",
          axis.title.x = element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          text=element_text(size=font_size)
    )
  # diurnal_microclimate
  
  fluxes_cumul <- data_long %>% 
    filter(
      name %in% c("ER", "GPP", "NEE")
    ) %>% 
    # pivot_wider(names_from = "name", values_from = "value") %>%
    group_by(site, name, turfID) %>%
    summarise(
      cumul_flux = sum(value)
    ) %>% 
    ggplot(aes(y = cumul_flux, x = site, fill = site, color = site)) +
    geom_boxplot(alpha = 0.5, outlier.shape = NA) +
    geom_jitter() +
    scale_fill_viridis(discrete=T, labels = c(
      hogsete = "Hogsete",
      joasete = "Joasete",
      liahovden = "Liahovden",
      vikesland = "Vikesland"
    )) +
    scale_color_viridis(discrete=T, labels = c(
      hogsete = "Hogsete",
      joasete = "Joasete",
      liahovden = "Liahovden",
      vikesland = "Vikesland"
    )) +
    scale_y_continuous(position = "right") +
    # scale_fill_manual(labels = c(
    #   hogsete = "Hogsete"
    # )) +
    labs(
      y="Cumulative fluxes",
      # x= "Value"
      fill = "Site",
      color = "Site"
    ) +
    facet_wrap(~name, scales = "free", ncol = 1) +
    # labs(x = "") +
    theme_bw() +
    theme(
      # legend.position = "none",
      strip.text.x = element_blank(),
      axis.title.x = element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      text=element_text(size=font_size)
      # axis.text.y=element_blank(),
      # axis.ticks.y=element_blank()
    )
  # theme(legend.position = "none", 
  #       axis.text.x = element_text(angle = 45,vjust = 1, hjust=1))
  # fluxes_cumul
  # 
  # ggplot(cum_fluxes, aes(y = total.flux, x = site, fill = site, color = site)) +
  # geom_boxplot(alpha = 0.5, outlier.shape = NA) +
  # geom_jitter() +
  # scale_fill_viridis(discrete=T)+
  # scale_color_viridis(discrete=T)+
  # facet_wrap(~type, scales = "free") +
  # labs(x = "") +
  # theme_bw() +
  # theme(legend.position = "none", 
  #       axis.text.x = element_text(angle = 45,vjust = 1, hjust=1))
  # library(patchwork)
  # full_figure <- diurnal | (density_microclimate / fluxes_cumul) +
  # full_figure <- (density_microclimate / fluxes_cumul) | diurnal +
  # full_figure <- (diurnal_microclimate / diurnal_fluxes) | (density_microclimate / fluxes_cumul) +
  full_figure <- diurnal_microclimate + density_microclimate + diurnal_fluxes + fluxes_cumul +
    plot_layout(guides = "collect",
                ncol = 2,
                widths = c(2, 1),
                heights = c(5, 3)
    ) +
    plot_annotation(tag_levels = 'A')
  
  return(full_figure)
}

plots_making(data_long, 9)
ggsave("PFTC6datapaper_figure.png", width = 14, height = 12, units = "in")

# full_figure <- ggarrange(
#   diurnal,
#   density_microclimate,
#   fluxes_cumul
#   # ncol = 2
# )













