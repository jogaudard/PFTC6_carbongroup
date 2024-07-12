library(tidyverse)
library(dataDownloader)

get_file(node = "pk4bg",
         file = "Three-D_metaturfID.csv",
         path = "raw_data",
         remote_path = "Site")

meta_seedclim <- tibble(
  turfID = c("TTC 101", "TTC 110", "TTC 115", "TTC 146", "TTC 140", "TTC 141"),
  origSiteID = c("Hogsete", "Hogsete", "Hogsete", "Vikesland", "Vikesland", "Vikesland"),
  destSiteID = c("Hogsete", "Hogsete", "Hogsete", "Vikesland", "Vikesland", "Vikesland"),
  warming = "A"
)

metaturf <- read_csv("raw_data/Three-D_metaturfID.csv") %>%
  select(warming, origSiteID, turfID, destSiteID) %>%
  mutate(
    origSiteID = str_replace_all(
      origSiteID,
      c("Lia" = "Liahovden" , "Joa" = "Hogsete", "Vik" = "Vikesland")
    ),
    destSiteID = str_replace_all(
      destSiteID,
      c("Lia" = "Liahovden" , "Joa" = "Joasete", "Vik" = "Vikesland")
    ),
    warming = str_replace_all(
      warming,
      c("ambient" = "A", "warming" = "W")
    )
  ) %>%
  bind_rows(meta_seedclim)
