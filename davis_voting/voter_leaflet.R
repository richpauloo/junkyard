# packages
library(leaflet)
library(leaflet.extras)
library(tidyverse)
library(sf)


# read in data
d <- read_csv("~/Github/junkyard/davis_voting/dat.csv")

# sanity check: ensure all unique Ids
nrow(d) == length(unique(d$`Response ID`))

# clean first row -- question number
d <- slice(d, -1)

# age of respondents
d[[16]] %>% 
  as.numeric() %>% 
  tibble(x=.) %>% 
  mutate(age = 2018-x) %>% 
  ggplot(aes(age)) + 
  geom_histogram(bins = 80) + 
  coord_cartesian(xlim = c(16, 50)) +
  labs(title = "Approximate age of respondents")

# subset for finished and not finished?
# not all students finished the survey, but presumably only students
# filled it out to begin with. We want a density of where students live
# to inform a voting block representative of students, so unfinshed
# surveys are still important
x <- table(d[2:nrow(d), "Finished"])

# the actual completion rate = 77%
# Thus, this survey mostly reflects the opinion of 1869 
# self-selecting students
x[2] / sum(x)


# grab important location data and create intersetions for geocoder
ds <- d[2:nrow(d) , c(24:26,28)]
colnames(ds) <- c("x1","x2","zip","n_people")
ds <- mutate(ds, 
             zip = ifelse(is.na(zip) | zip == "NA", "95616", zip),
             zip = as.numeric(zip),
             geo = paste0(x1, " and ", x2, ", Davis, CA ", zip))

# geocode with OSM -- takes a while. Then save it to avoid re-geocoding
ggmap::register_google("AIzaSyC_dLbZhGkf-tb81oH-xeRk_kANAL2fcu8")
l <- lapply(ds$geo, purrr::possibly(ggmap::geocode, NA_real_), source = "google")
textme()

# combine into dataframe
ls <- do.call(rbind.data.frame, l)
ls <- bind_cols(ls, n_people = as.numeric(ds$n_people))
write_rds(ls, "ls.rds")


# sainty check: number of failed geocodes
# 3 failed addresses
nrow(filter(ls, is.na(lon) | is.na(lat)))

# remove failing addresses
ls <- filter(ls, !is.na(lon) & !is.na(lat))

# convert to sf
lsf <- st_as_sf(ls, coords = c("lon","lat"), crs = 4326)

# crop to city of Davis
#shp  <- mapedit::editMap(mapview::mapview(lsf)) 
#write_rds(shp, "shp.rds")
lsfd <- lsf[shp$drawn, ]
#write_rds(lsfd, "lsfd.rds")
  
mapview::mapview(lsfd)

# map
leaflet(lsfd) %>%
  setView(lat = mean(st_coordinates(lsfd)[, 2]),
          lng = mean(st_coordinates(lsfd)[, 1]),
          zoom = 10) %>% 
  addProviderTiles(providers$CartoDB.DarkMatter, group = "Carto") %>% 
  addProviderTiles(providers$Esri.WorldImagery, group = "World") %>% 
  addProviderTiles(providers$OpenStreetMap, group = "Street") %>% 
  addMarkers(clusterOptions = markerClusterOptions()) %>% 
  addDrawToolbar(
    targetGroup = "my_polygon", 
    circleOptions = FALSE, 
    polylineOptions = FALSE,
    rectangleOptions = FALSE,
    circleMarkerOptions = FALSE,
    markerOptions = FALSE,
    editOptions = editToolbarOptions(
      selectedPathOptions = selectedPathOptions()
    )
  )  %>%
  addLayersControl(
    overlayGroups = "my_polygon",
    baseGroups = c("Carto", "World", "Street"),
    options = layersControlOptions(collapsed = FALSE, position = "bottomright")
  ) %>%
  addStyleEditor() %>% 
  addFullscreenControl(position = "topleft", 
                       pseudoFullscreen = FALSE) %>% 
  addMeasurePathToolbar(options =
                          measurePathOptions(imperial = TRUE,
                                             minPixelDistance = 100,
                                             showDistances = FALSE)
                        )
  