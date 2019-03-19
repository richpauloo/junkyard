library(xml2)
library(httr)
library(dplyr)
library(stringr)
library(tidyr)


##########################################################################
# check for active stations in the central valley
##########################################################################

library(sp)

s <- GET("http://et.water.ca.gov/api/station")
s <- content(s)

cn <- s$Stations[[1]] %>% names() %>% .[-14]
l  <- lapply(s$Stations, function(x) as.data.frame(x[-14], col.names = cn)) %>% 
  do.call(rbind.data.frame, .)

# drop unnecerssary data and clean lat/lon
l <- select(l, StationNbr, IsActive, IsEtoStation, HmsLatitude, HmsLongitude) %>% 
  separate(col = HmsLatitude, into = letters[1:2], "/ ") %>% 
  separate(col = HmsLongitude, into = letters[3:4], "/ ") %>% 
  select(- c("a", "c")) %>% 
  rename(lat = b, lng = d) %>% 
  mutate(lat = as.numeric(lat), lng = as.numeric(lng))

# load central valley shapefile
ll <- "+init=epsg:4269 +proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0"
cv <- raster::shapefile("C:/Users/rpauloo/Documents/Github/junkyard/cimis/data/Alluvial_Bnd.shp")
cv <- spTransform(cv, ll)

# make stations into spatial object
spdf <- SpatialPointsDataFrame(coords      = as.matrix(l[,c("lng","lat")]), 
                               data        = l, 
                               proj4string = CRS(ll))

# sanity check
plot(cv); points(spdf)

# subset of points within cv
spdf_in <- spdf[cv, ]

# sanity check
plot(cv); points(spdf_in, col = "red", pch = 16)

# subset for active stations that record Eto

# sanity check
plot(cv); points(spdf_in[(spdf_in$IsActive == 'True' & 
                            spdf_in$IsEtoStation == 'True'), ], 
                 col = "red", pch = 16)

# active station numbers
active_stations <- spdf_in[(spdf_in$IsActive == 'True' & 
                              spdf_in$IsEtoStation == 'True'), ]@data %>% 
  pull(StationNbr) %>% 
  as.numeric()



##########################################################################
# query Eto at active stations for the last month
##########################################################################



# query parameters
key       <- "43e46605-8d96-491e-aa09-be9a5ca4bcf3"
targets   <- "2,125"
startdate <- "2019-03-01"
enddate   <- "2019-03-03"
dataitems <- "day-asce-eto"

# url
g <- paste0("http://et.water.ca.gov/api/data?appKey=",
            key,
            "&targets=",
            targets, 
            "&startDate=",
            startdate,
            "&endDate=",
            enddate,
            "&dataItems=",
            dataitems)

r <- GET(g)     # GET HTTP response
r <- content(r) # response content as list

# get data from list
d <- r$Data$Providers[[1]]$Records %>% 
  lapply(as.data.frame) %>% 
  do.call(rbind.data.frame, .) 







