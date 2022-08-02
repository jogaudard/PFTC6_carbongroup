library(tidyverse)
library(dataDownloader)

get_file(node = "pk4bg",
         file = "Three-D_metaturfID.csv",
         path = "raw_data",
         remote_path = "Site")

meta_seedclim <- tibble(
  turfID = c("TTC 101", "TTC 110", "TTC 115", "TTC 146", "TTC 140", "TTC 141"),
  site = c("hogsete", "hogsete", "hogsete", "vikesland", "vikesland", "vikesland"),
  warming = "ambient"
)

metaturf <- read_csv("raw_data/Three-D_metaturfID.csv") %>% 
  select(warming, origSiteID, turfID) %>% 
  rename(
    site = origSiteID
  ) %>% 
  mutate(
    site = str_replace_all(
      site,
      c("Lia" = "liahovden" , "Joa" = "joasete", "Vik" = "vikesland")
    ),
    warming = str_replace_all(
      warming,
      c("W" = "transplant", "A" = "ambient")
    )
  ) %>% 
  bind_rows(meta_seedclim)





