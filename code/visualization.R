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
         file = "PFTC6_microclimate_allsites_2022.csv", #this version of the file has big holes, trying to figure out with Hilary what happened
         path = "clean_data",
         remote_path = "microclimate")

fluxes <- read_csv("clean_data/PFTC6_24h_cflux_allsites_2022.csv")

microclimate <- read_csv("clean_data/PFTC6_microclimate_allsites_2022.csv") %>% 
  mutate(
    site = str_to_lower(site)
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
  filter(
    site == "vikesland"
  ) %>%
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



# diurnal and density together --------------------------------------------




