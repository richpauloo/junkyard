library(tidyverse)
library(fs)
library(glue)
library(mapview)
library(sf)

# data and minimal data for spatial plotting
d <- read_csv("out/2024-05-27-dwr-continuous.csv")
ds <- d %>% 
  group_by(site_code) %>% 
  slice(1) %>% 
  ungroup() %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

# mostly Northern Californian basins
ds %>% mapview()

# how many of these site codes are part of GSP monitoring networks
# https://sgma.water.ca.gov/portal/gsp/monitoringsites
# See "Export to CSV" button - RSelenium later if needed
gsp <- read_csv("data/2024-05-27/Groundwater Wells Export.csv") %>% 
  janitor::clean_names()

dg <- d %>% filter(site_code %in% unique(gsp$site_code))
cat(length(unique(d$site_code)), "IDs in DWR continuous data.\n")
cat(
  length(unique(dg$site_code)), 
  "IDs in DWR continuous data after filtering by GSP.\n"
)

dg %>% 
  group_by(site_code) %>% 
  slice(1) %>% 
  ungroup() %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
  mapview()

# hydrographs
ids <- unique(dg$site_code)
dg %>% 
  filter(site_code %in% ids[1:10]) %>%
  ggplot(aes(msmt_date, gse_gwe)) +
  geom_line() +
  facet_wrap(~site_code, ncol = 2, scales = "free")
