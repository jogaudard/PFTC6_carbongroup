# use this script to clean all the sites, graph all the fluxes, and produce a clean dataset off the fluxes

# sourcing what we need ---------------------------------------------------


# let's call functions and packages here instead of in the individual scripts

my_packages <- c("dataDownloader",
                 "tidyverse",
                 "lubridate",
                 "broom",
                 "zoo",
                 "hms",
                 "stringr"
                 )

lapply(my_packages, library, character.only = TRUE)

sources <- list("code/functions.R",
                "code/metaturf.R",
                "code/cleaning_vikesland.R",
                "code/cleaning_liahovden.R",
                "code/cleaning_joasete.R",
                "code/cleaning_hogsete.R"
                )

sapply(sources, source)

# source(exprs = sources,
#        #   "code/cleaning_liahovden.R",
#        #   "code/cleaning_joasete.R",
#        #   "code/cleaning_hogsete.R"
#        #   ),
#        # local = FALSE
#        # evaluated = TRUE
#        echo = TRUE
#        )


# creating full dataset ---------------------------------------------------

# cflux_vikesland_corrected <- cflux_vikesland_corrected %>%
#   mutate(
#     site = "Vik"
#   )
#
# cflux_liahovden_corrected <- cflux_liahovden_corrected %>%
#   mutate(
#     site = "Lia"
#   )
#
# cflux_joasete_corrected <- cflux_joasete_corrected %>%
#   mutate(
#     site = "Joa"
#   )
#
# cflux_hogsete_corrected <- cflux_hogsete_corrected %>%
#   mutate(
#     site = "Hog"
#   )

# Update the site names in metaturf
# metaturf_clean <- metaturf |>
#   mutate_all(funs(str_replace(., "Lia", "Liahovden"))) |>
#   mutate_all(funs(str_replace(., "Joa", "Joasete"))) |>
#   mutate_all(funs(str_replace(., "Hog", "Hogsete"))) |>
#   mutate_all(funs(str_replace(., "Vik", "Vikesland")))

cflux_all_clean <- bind_rows(
  cflux_vikesland_corrected,
  cflux_liahovden_corrected,
  cflux_joasete_corrected,
  cflux_hogsete_corrected
) %>%
left_join(metaturf)
  # left_join(metaturf_clean)

# There were missing round due to issues on the field
# # We are missing a round in Joa and Lia
# # let's try to find them
#
# round_flux <- fluxes %>%
#   mutate(
#     round_time = hour(datetime)
#   ) %>%
#   select(destSiteID, turfID, round_time) %>%
#   distinct() %>%
#   arrange(round_time) %>%
#   group_by(turfID) %>%
#   # rowid_to_column("rowID") %>%
#   mutate(
#     rowID = row_number()
#   ) %>%
#   ungroup() %>%
#   pivot_wider(names_from = turfID, values_from = round_time)

# Joa is missing the round starting at 2022-07-28 18:20:00
# Lia is missing the round starting at 2022-07-28 00:30:00

# we need to create those two datasets.
missing_joa <- cflux_all_clean %>%
  filter(
    destination == "Joasete"
    # & type != "GPP"
  ) %>%
  select(destination, turfID, origin, warming) %>%
  distinct() %>%
  mutate(
    # destSiteID = factor(destSiteID, level = c("81 AN1C 81", "3 WN1C 85", "")) # no need, turfID already in right order
    NEE = seq(ymd_hms("2022-07-28 18:20:00"), ymd_hms("2022-07-28 19:19:00"), by = 600), #fluxes were measured about every 10 minutes
    ER = seq(ymd_hms("2022-07-28 18:25:00"), ymd_hms("2022-07-28 19:19:00"), by = 600),
    GPP = NEE
  )

missing_lia <- cflux_all_clean %>%
  filter(
    destination == "Liahovden"
  ) %>%
  select(destination, turfID, origin, warming) %>%
  distinct() %>%
  mutate(
    NEE = seq(ymd_hms("2022-07-28 00:30:00"), ymd_hms("2022-07-28 00:59:00"), by = 600), #fluxes were measured about every 10 minutes
    ER = seq(ymd_hms("2022-07-28 00:35:00"), ymd_hms("2022-07-28 00:59:00"), by = 600),
    GPP = NEE
  )

missing_rounds <- bind_rows(missing_joa, missing_lia) %>%
  pivot_longer(cols = c(NEE, ER, GPP), names_to = "type", values_to = "datetime") %>%
  mutate(
    flag = "missing round",
    time = as_hms(datetime)
  )

# we put them into the flux dataset

cflux_all_clean <- cflux_all_clean %>%
  bind_rows(missing_rounds) |>
  # Reorganise data
  arrange(datetime, time, origin, destination, turfID, warming, type, fluxID, flux, flux_noflag, flux_corrected, PARavg,
          temp_soil, temp_airavg, flag)


write_csv(cflux_all_clean, "clean_data/PFTC6_24h_cflux_allsites_2022.csv")



# to write number of flags in data paper
cflux_all_clean %>% select(flag, flux_corrected) %>% count(flag)
