library(tidyverse)
library(dataDownloader)

get_file(node = "pk4bg",
         file = "Three-D_metaturfID.csv",
         path = "raw_data",
         remote_path = "Site")

meta_seedclim <- tibble(
  turfID = c("TTC 101", "TTC 110", "TTC 115", "TTC 146", "TTC 140", "TTC 141"),
  origin = c("Hogsete", "Hogsete", "Hogsete", "Vikesland", "Vikesland", "Vikesland"),
  destination = c("Hogsete", "Hogsete", "Hogsete", "Vikesland", "Vikesland", "Vikesland"),
  warming = "ambient"
)

metaturf <- read_csv("raw_data/Three-D_metaturfID.csv") %>%
  select(warming, origSiteID, turfID, destSiteID) %>%
  rename(
    origin = origSiteID,
    destination = destSiteID
  ) %>%
  mutate(
    origin = str_replace_all(
      origin,
      c("Lia" = "Liahovden" , "Joa" = "Hoasete", "Vik" = "Vikesland")
    ),
    destination = str_replace_all(
      destination,
      c("Lia" = "Liahovden" , "Joa" = "Joasete", "Vik" = "Vikesland")
    ),
    warming = str_replace_all(
      warming,
      c("W" = "transplant", "A" = "ambient")
    )
  ) %>%
  bind_rows(meta_seedclim)





