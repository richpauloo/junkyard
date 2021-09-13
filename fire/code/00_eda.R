library(here)
library(tidyverse)
library(sf)
library(mapview)

# fire data from
# https://frap.fire.ca.gov/mapping/gis-data/

# standard crs to use
epsg <- 3310 # spatial operations
ll   <- 4269 # visualization 

# fire perimeters, last updated April, 2021
s <- st_read(here("data","fire20_1.gdb","a000000af.gdbtable"))
mapview(arrange(s, -GIS_ACRES)[1:100,])

# facilities to fight fire
f <- st_read(here("data","facility21_1.gdb","a00000009.gdbtable"))
mapview(f, zcol = "TYPE")

# WUIs

# soil erosion zones

# fire threat
t <- st_read(here("data","fthrt14_2.gdb","a00000004.gdbtable"))
mapview(t, zcol = "TYPE")