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

functions = dir(path = "R_code/functions", full.names = TRUE)

sapply(functions, source)

sources <- list("R_code/data_cleaning/make_metadata.R",
                "R_code/data_cleaning/cleaning_cflux_site_vikesland.R",
                "R_code/data_cleaning/cleaning_cflux_site_liahovden.R",
                "R_code/data_cleaning/cleaning_cflux_site_joasete.R",
                "R_code/data_cleaning/cleaning_cflux_site_hogsete.R"
                )

sapply(sources, source)

# creating full dataset ---------------------------------------------------
cflux_all_clean <- bind_rows(
  cflux_vikesland_corrected,
  cflux_liahovden_corrected,
  cflux_joasete_corrected,
  cflux_hogsete_corrected
) %>%
  left_join(metaturf)

# There were missing round due to issues on the field
# # We are missing a round in Joa and Lia
# we need to create those two datasets.
missing_joa <- cflux_all_clean %>%
  filter(
    destSiteID == "Joasete"
    # & type != "GPP"
  ) %>%
  select(destSiteID, turfID, origSiteID, warming) %>%
  distinct() %>%
  mutate(
    # destSiteID = factor(destSiteID, level = c("81 AN1C 81", "3 WN1C 85", "")) # no need, turfID already in right order
    NEE = seq(ymd_hms("2022-07-28 18:20:00"), ymd_hms("2022-07-28 19:19:00"), by = 600), #fluxes were measured about every 10 minutes
    ER = seq(ymd_hms("2022-07-28 18:25:00"), ymd_hms("2022-07-28 19:19:00"), by = 600),
    GPP = NEE
  )

missing_lia <- cflux_all_clean %>%
  filter(
    destSiteID == "Liahovden"
  ) %>%
  select(destSiteID, turfID, origSiteID, warming) %>%
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
  # Rename column with relevant flux values
  rename(flux_value = flux_corrected) |>
  # Remove data columns that aren't in the clean dataset
  select(-c(flux, flux_noflag)) |>
  # Reorganise data
  relocate(datetime, time, origSiteID, destSiteID, turfID, warming, type, fluxID, flux_value, PARavg,
           temp_soil, temp_airavg, flag)


write_csv(cflux_all_clean, "clean_data/PFTC6_cflux_2022.csv")

# to write number of flags in data paper
cflux_all_clean %>% select(flag, flux_value) %>% count(flag)
