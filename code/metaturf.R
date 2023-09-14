library(tidyverse)
library(dataDownloader)

get_file(node = "pk4bg",
         file = "Three-D_metaturfID.csv",
         path = "raw_data",
         remote_path = "Site")

meta_seedclim <- tibble(
  turfID = c("TTC 101", "TTC 110", "TTC 115", "TTC 146", "TTC 140", "TTC 141"),
  origSiteID = c("Hog", "Hog", "Hog", "Vik", "Vik", "Vik"),
  destSiteID = c("Hog", "Hog", "Hog", "Vik", "Vik", "Vik"),
  warming = "A"
)

# Harmonized with PFTC6 data paper
metaturf <- read_csv("raw_data/Three-D_metaturfID.csv") %>% 
  select(warming, origSiteID, turfID, destSiteID) %>% 
  bind_rows(meta_seedclim)


# Archived code ----
# meta_seedclim <- tibble(
#   turfID = c("TTC 101", "TTC 110", "TTC 115", "TTC 146", "TTC 140", "TTC 141"),
#   origin = c("hogsete", "hogsete", "hogsete", "vikesland", "vikesland", "vikesland"),
#   destination = c("hogsete", "hogsete", "hogsete", "vikesland", "vikesland", "vikesland"),
#   warming = "ambient"
# )
# 
# metaturf <- read_csv("raw_data/Three-D_metaturfID.csv") %>% 
#   select(warming, origSiteID, turfID, destSiteID) %>% 
#   rename(
#     origin = origSiteID,
#     destination = destSiteID
#   ) %>% 
#   mutate(
#     origin = str_replace_all(
#       origin,
#       c("Lia" = "liahovden" , "Joa" = "joasete", "Vik" = "vikesland")
#     ),
#     destination = str_replace_all(
#       destination,
#       c("Lia" = "liahovden" , "Joa" = "joasete", "Vik" = "vikesland")
#     ),
#     warming = str_replace_all(
#       warming,
#       c("W" = "transplant", "A" = "ambient")
#     )
#   ) %>% 
#   bind_rows(meta_seedclim)










