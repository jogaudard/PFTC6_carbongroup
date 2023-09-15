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
    # site = str_to_lower(site),
    # site = as_factor(site)
    destSiteID = as_factor(destSiteID)
  )

# need to cut fluxes so we match the time window of the fluxes

fluxes_startstop <- fluxes %>% 
  group_by(destSiteID) %>% 
  mutate(
    start = min(datetime),
    stop = max(datetime)
  ) %>% 
  select(destSiteID, start, stop) %>% 
  unique()

microclimate <- microclimate %>% 
  left_join(fluxes_startstop, by = "destSiteID") %>% 
  filter(
    datetime <= stop
    & datetime >= start
    # we need to do some funny stuff for joasete because some of the fluxes were measured the day after following equipment failure
    & !(datetime > ymd_hms("2022-07-29T03:10:00") & datetime < ymd_hms("2022-07-30T03:10:00"))
  )



# arranging the data ------------------------------------------------------

# first we need to arrange the data in a weirdly mixed long format, microclimate and fluxes together
data_long <- full_join(microclimate, fluxes, by = c("datetime", "value" = "flux_corrected", "destSiteID", "turfID")) %>%
  mutate(
    PAR = case_when(
      type == "GPP" ~ PARavg,
      type == "ER" ~ NA_real_ #because we want to show the ambient PAR, we get rid of the ER ones (measured inside the dark chamber)
    ),
    type = case_when(
      is.na(type) ~ sensor, #include the microclimate sensor in the type column
      TRUE ~ type
    )
    # site = str_to_lower(site)
  ) %>%
  # filter(
  #   type != "NEE" #just in case we are not interested in NEE
  # ) %>% 
  select(type, PAR, datetime, destSiteID, value, turfID) %>% 
  rowid_to_column("rowID") %>% #makes each rows unique, helps with pivot wider
  pivot_wider(names_from = "type", values_from = "value") %>% #just a trick to get all the value in the same column (PAR has its own column)
  pivot_longer(cols = c(PAR, GPP, ER, NEE, air_temperature, soil_temperature, ground_temperature, soil_moisture)) %>% 
  drop_na(value) %>% #because of the two pivots we created a lot of empty useless rows
  mutate( #making things easier to make graphs later
    name = as_factor(name),
    time = hms::as_hms(datetime),
    name = factor(name, levels = c("air_temperature", "ground_temperature", "soil_temperature", "soil_moisture", "PAR", "ER", "NEE", "GPP")), # we need to make sure everything is in the same order
    destSiteID = factor(destSiteID, levels = c("Vik", "Hog", "Joa", "Lia"))
  )

# diurnal and density together (the figure for the datapaper) --------------------------------------------

# Aud's idea to "save time" (haha):
# put everything in a function, that way we can re generate the graphs all together when we change something
# edit: it was actually fast and it is super practical
plots_making <- function(data_long, font_size)
{
  density_microclimate <- data_long %>% 
    filter( #we just want microclimate
      name %in% c("air_temperature", "ground_temperature", "soil_moisture", "soil_temperature", "PAR")
    ) %>% 
    ggplot(aes(x=value, fill=destSiteID)) +
    geom_density(alpha=0.6, linewidth = 0.8) +
    scale_fill_viridis(discrete=T) +
    scale_y_continuous(position = "right") +
    facet_wrap(name~.,labeller = label_parsed, scales="free", ncol = 1) +
    labs(
      y="Density",
      x= "Microclimate value"
    ) +
    theme_bw() +
    theme(legend.position="none",
          strip.text.x = element_blank(),
          text=element_text(size=font_size)
    )
  
  diurnal_fluxes <- data_long %>% 
    filter(
      # name != "NEE" # in case we are not intersted in NEE
      name %in% c("ER", "GPP", "NEE")
    ) %>%
    ggplot(aes(time, value, color=destSiteID)) +
    geom_point(size=0.05) +
    geom_smooth(method = "loess", span = 0.3) +
    facet_grid(name~., scales = "free") +
    scale_color_viridis(discrete=T) +
    theme_bw() +
    theme(legend.position="none",
          text=element_text(size=font_size)) +
    labs(
      y="Fluxes",
      x= "Time"
    )
  
  
  diurnal_microclimate <- data_long %>% 
    filter(
      name %in% c("air_temperature", "ground_temperature", "soil_moisture", "soil_temperature", "PAR")
    ) %>%
    ggplot(aes(time, value, color=destSiteID)) +
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
    theme_bw() +
    labs(
      y="Microclimate value"
    ) +
    theme(legend.position="none",
          axis.title.x = element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          text=element_text(size=font_size)
    )
  
  fluxes_cumul <- data_long %>% 
    filter(
      name %in% c("ER", "GPP", "NEE")
    ) %>% 
    group_by(destSiteID, name, turfID) %>% 
    summarise(
      cumul_flux = sum(value) # we sum each fluxes for each turfID
    ) %>% 
    ggplot(aes(y = cumul_flux, x = destSiteID, fill = destSiteID, color = destSiteID)) +
    geom_boxplot(alpha = 0.5, outlier.shape = NA) +
    geom_jitter() +
    scale_fill_viridis(discrete=T, labels = c( #this is the plot providing the legend to the patchwork
      Hog = "Hogsete",
      Joa = "Joasete",
      Lia = "Liahovden",
      Vik = "Vikesland"
    )) +
    scale_color_viridis(discrete=T, labels = c(
      Hog = "Hogsete",
      Joa = "Joasete",
      Lia = "Liahovden",
      Vik = "Vikesland"
    )) +
    scale_y_continuous(position = "right") +
    labs(
      y="Cumulative fluxes",
      fill = "Site",
      color = "Site"
    ) +
    facet_wrap(~name, scales = "free", ncol = 1) +
    theme_bw() +
    theme(
      strip.text.x = element_blank(),
      axis.title.x = element_blank(),
      # axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      text=element_text(size=font_size)
    )
  
  patchwork <- diurnal_microclimate + density_microclimate + diurnal_fluxes + fluxes_cumul +
    plot_layout(guides = "collect",
                ncol = 2,
                widths = c(2, 1),
                heights = c(5, 3) #this ratio makes sure all the diurnals have the same heigt
    ) +
    plot_annotation(tag_levels = 'A')
  
  return(patchwork)
}

plots_making(data_long, 11)
ggsave("PFTC6datapaper_figure.png", width = 14, height = 12, units = "in")






