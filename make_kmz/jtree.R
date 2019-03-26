library(rvest)
library(dplyr)
library(tibble)

# scape for waypoint, ll, and description
url <- "https://www.backpacker.com/trips/joshua-tree-national-park-california-riding-and-hiking-trail"

d <- read_html(url)
d <- d %>% html_nodes("p") %>% html_text()

# waypoints, lat lng coords, and descriptions
w  <- d[grepl(pattern = "WPT", d)]
ll <- d[(which(grepl(pattern = "WPT", d)) + 1)]
ds <- d[(which(grepl(pattern = "WPT", d)) + 2)]

# make the data frame
df <- tibble(w, ll, ds) %>% 
  tidyr::separate(ll, into = c(NA, "ll"), sep = ": ") %>% 
  tidyr::separate(ll, into = c("lat", "lng"), sep = ", ") %>% 
  mutate(lat = as.numeric(lat),
         lng = as.numeric(lng),
         wds = paste(w, ds, sep = ": ")) 

# make spatial
library(sp)
coords <- df[, 3:2]
data   <- df %>% select(w, ds)
pro    <- "+proj=longlat +datum=WGS84"

spdf <- SpatialPointsDataFrame(coords = coords,
                               data   = data, 
                               proj4string = CRS(pro))

# make leaflet
library(leaflet)

providers <- c("Esri.WorldTopoMap", "Stamen.TopOSMFeatures", "Stamen.TopOSMRelief", "Esri.WorldImagery")
map = leaflet(spdf) %>% 
  addTiles() %>% 
  addMarkers(label = paste(w, ds, sep = ": "))
for(i in 1:length(providers)){
  map = map %>% addProviderTiles(providers[i], group = providers[i])
}

map = map %>% addLayersControl(
  baseGroups = providers,
  options = layersControlOptions(collapsed = FALSE))

map

# save HTML
library(htmlwidgets)
saveWidget(map, file = "jtriz.html")

# save to kmz
library(rgdal)
writeOGR(spdf, "jtriz.kml", layer="wds", driver="KML") 
