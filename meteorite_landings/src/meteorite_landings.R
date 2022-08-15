library(tidyverse)
library(sf)
library(leaflet)
library(rnaturalearth)
library(rnaturalearthdata)
library(gganimate) 

world <- ne_countries(scale = "medium", returnclass = "sf")

# https://data.nasa.gov/Space-Science/Meteorite-Landings/gh4g-9sfh

d <- read_csv("~/Downloads/Meteorite_Landings.csv") %>% 
  filter(!is.na(reclat) & !is.na(reclong) & !is.na(year) & 
           year > 1800 & year < 2022) %>% 
  st_as_sf(coords = c("reclong", "reclat"), crs = 4326) %>% 
  filter(id != 32789) %>% 
  mutate(m = log(`mass (g)`)) %>% 
  filter(!is.infinite(m)) %>% 
  mutate(m = m/max(m, na.rm = TRUE),
         year = as.integer(year),
         hack = paste0(sample(letters, 1), rnorm(1, 5, 1)))

yrs <- sort(unique(d$year))
p  <- vector("list", length = length(yrs))

for(i in seq_along(yrs)){
  p[[i]] <- ggplot() + 
    geom_sf(data = world, lwd = 0, fill = "lightgrey") + 
    geom_sf(data = filter(d, year <= yrs[i]), 
            color = "darkred", alpha = 0.1) + 
    labs(
      title   = glue::glue("Global Meteorite Landings (Year: {yrs[i]})"), 
      caption = "NASA Meteorite Landings Dataset (https://data.nasa.gov/Space-Science/Meteorite-Landings/gh4g-9sfh)"
    ) +
    theme_void()
}

walk2(p, yrs, ~ggsave(glue::glue("~/Desktop/meteorite_landings/{.y}.png"), .x))      
system("/usr/local/bin/convert -delay 05 /Users/richpauloo/Desktop/meteorite_landings/*.png /Users/richpauloo/Desktop/meteorite_landings.gif")
