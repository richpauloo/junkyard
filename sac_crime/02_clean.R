library(tidyverse)
library(here)
library(sf)
library(USAboundaries)

# read in all csv files and convert to sf
d <- list.files(here("data"), full.names = TRUE) %>% 
  read_csv(id = "path") %>% 
  filter(!is.na(X_Coord) & !is.na(Y_Coord)) %>% 
  st_as_sf(coords = c("X_Coord", "Y_Coord"))

# guess CRS and set it
best_crs <- crsuggest::guess_crs(head(d), 
                                 c(-121.211077, 38.533745)) %>% 
  slice(1) %>% 
  pull(crs_code) %>% 
  as.numeric()

st_crs(d) <- best_crs

# transform for intersection
d <- st_transform(d, 3310)

# sanity check
mapview::mapview(head(d, 100))

# Sac county spatial data
sac <- USAboundaries::us_counties(states = "CA") %>% 
  filter(name == "Sacramento") %>% 
  st_transform(3310)

# intersection
d2 <- st_intersection(d, sac)

# compute and print dropped rows
diff <- nrow(d) - nrow(d2)
cat("Dropped", diff, "(", round(diff/nrow(d) * 100, 2), "%)",
    "observations outside of Sac county.")

# write
dir.create(here("data_output"))
write_rds(d2, here("data_output/preprocessed.rds"))

