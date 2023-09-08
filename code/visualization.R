my_packages <- c("dataDownloader",
                 "tidyverse",
                 "lubridate",
                 "ggpubr"
)

lapply(my_packages, library, character.only = TRUE) 

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


# arranging the data ------------------------------------------------------

# first we need to arrange the data in a weirdly mixed long format
data_long <- full_join(microclimate, fluxes, by = c("datetime", "value" = "flux_corrected", "site")) %>% 
  


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
  filter(
    # is.na(type) |
    type != "NEE"
  ) %>% 
  select(type, PAR, datetime, site, value) %>% 
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
  pivot_longer(cols = c(PAR, GPP, ER, air_temperature, soil_temperature, ground_temperature, soil_moisture)) %>% 
  drop_na(value) %>% 
  mutate(
    name = as_factor(name),
    time = hms::as_hms(datetime)
  )

# liahovden ---------------------------------------------------------------


# cumulative graph --------------------------------------------------------

cum_fluxes <- fluxes %>% 
  pivot_wider(names_from = "type", values_from = "flux_corrected") %>% 
  mutate(
    cum_flux_corr_ER = replace(ER, !is.na(ER), cumsum(na.omit(ER))),
    cum_flux_corr_NEE = replace(NEE, !is.na(NEE), cumsum(na.omit(NEE))),
    cum_flux_corr_GPP = replace(GPP, !is.na(GPP), cumsum(na.omit(GPP)))
  ) %>% 
  select(!c(ER, NEE, GPP)) %>% 
  pivot_longer(cols = c(cum_flux_corr_ER, cum_flux_corr_NEE, cum_flux_corr_GPP), values_to = "cum_flux_corr", names_to = "type") %>% 
  mutate(
    type = str_replace_all(
      type,
      c(
        "cum_flux_corr_ER" = "ER",
        "cum_flux_corr_NEE" = "NEE",
        "cum_flux_corr_GPP" = "GPP"
      )
    )
  ) %>% 
  drop_na(cum_flux_corr)


  ggplot(aes(x = time, y = cum_flux_corr)) +
  geom_line() +
  geom_point() +
  facet_grid(type ~ site, scales = "free")

# diurnal -----------------------------------------------------------------



data_long %>% 
  # filter(
  #   site == "liahovden"
  # ) %>% 
  ggplot(aes(time, value, color=site)) +
  geom_point(size=0.05) +
  geom_smooth() +
  facet_grid(name~., scales = "free")


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



