# use this script to clean all the sites, graph all the fluxes, and produce a clean dataset off the fluxes

# sourcing what we need ---------------------------------------------------


# let's call functions and packages here instead of in the individual scripts

my_packages <- c("dataDownloader",
                 "tidyverse",
                 "lubridate",
                 "broom"
                 )

lapply(my_packages, library, character.only = TRUE) 

sources <- list("code/functions.R",
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

cflux_vikesland_corrected <- cflux_vikesland_corrected %>% 
  mutate(
    site = "vikesland"
  )

cflux_liahovden_corrected <- cflux_liahovden_corrected %>% 
  mutate(
    site = "liahovden"
  )

cflux_joasete_corrected <- cflux_joasete_corrected %>% 
  mutate(
    site = "joasete"
  )

cflux_hogsete_corrected <- cflux_hogsete_corrected %>% 
  mutate(
    site = "hogsete"
  )

cflux_all_clean <- bind_rows(
  cflux_vikesland_corrected,
  cflux_liahovden_corrected,
  cflux_joasete_corrected,
  cflux_hogsete_corrected
)

write_csv(cflux_all_clean, "clean_data/PFTC6_24h_cflux_allsites_2022.csv")



# to test the quality threshold -------------------------------------------













