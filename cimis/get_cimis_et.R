##########################################################################
# This script scrapes the last month's ET data from active stations in
# California's Central Valley from CIMIS:
#    * web: https://cimis.water.ca.gov/
#    * API: https://et.water.ca.gov/Rest/Index
##########################################################################

# packages
library(xml2)
library(httr)
library(dplyr)
library(stringr)
library(tidyr)
library(sp)
library(lubridate)

##########################################################################
# check for active stations in the central valley
##########################################################################

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
  mutate(lat        = as.numeric(lat), 
         lng        = as.numeric(lng),
         StationNbr = as.numeric(as.character(StationNbr)))

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

# all stations including currently inactive
active_stations <- spdf_in[spdf_in$IsEtoStation == 'True', ]@data %>% 
  pull(StationNbr) %>% 
  as.numeric()



##########################################################################
# query Eto at active stations for the last month
##########################################################################

# length of ET window (today's date minus this many days)
len <- 30

# query parameters
key       <- ""
targets   <- paste(active_stations, collapse = ",")
startdate <- "2001-01-01" #as.character(ymd(substr(Sys.time(), 1, 10)) - len) 
enddate   <- substr(Sys.time(), 1, 10)
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
# deal with variable number of unequal column lengths using 
# data.table::rbindlist(. , fill = TRUE) and do light cleaning
d <- r$Data$Providers[[1]]$Records %>% 
  lapply(., unlist) %>% 
  lapply(., as.data.frame) %>% 
  lapply(., t) %>% 
  lapply(., as.data.frame) %>% 
  data.table::rbindlist(., fill = TRUE) %>% 
  rename(date    = Date, 
         julian  = Julian, 
         station = Station, 
         et      = DayAsceEto.Value) %>% 
  mutate(date    = ymd(date), 
         julian  = as.numeric(as.character(julian)), 
         station = as.numeric(as.character(station)),
         et      = as.numeric(as.character(et))) %>% 
  select(date, julian, station, et)
  
# join to station info
d <- left_join(d, l, by = c("station" = "StationNbr"))


##########################################################################
# split into list by date/julian, interpolate, and save raster output
##########################################################################

# make into spatial object
dsp <- SpatialPointsDataFrame(coords      = as.matrix(d[,c("lng","lat")]), 
                              data        = d, 
                              proj4string = CRS(ll))
dsp <- split(dsp, dsp$date)

# interpolate and turn raster into data frame
tps <- vector("list", length(dsp))

library(fields)
library(raster)

dtt <- names(dsp)
for(i in 1:length(tps)){
  mod      <- Tps(dsp[[i]]@data[, c("lng", "lat")], dsp[[i]]@data[, "et"])
  r        <- raster(cv, nrow = 100, ncol = 100) # res: 0.002008855, 0.002061255  (x, y)
  r        <- interpolate(r, mod)
  r        <- mask(r, cv)
  tps[[i]] <- as.data.frame(r, xy = TRUE) %>% 
    rename(et   = layer) %>% 
    mutate(date = ymd(dtt[i]))
}

tpsdf <- bind_rows(tps) %>% filter(!is.na(et))

# spplot style binning
brks <- seq(min(tpsdf$et), max(tpsdf$et), (max(tpsdf$et) - min(tpsdf$et))/16) %>% round(2)
labs <- paste(brks[1:length(brks)-1], brks[2:length(brks)], sep = " - ")
tpsdf$etb <- cut(tpsdf$et, breaks = brks, labels = labs)

library(gganimate)
gifa <- ggplot(tpsdf, aes(x, y)) +
  geom_raster(aes(fill = et)) + 
  geom_path(data = rename(fortify(cv), x = long, y = lat), 
            aes(x,y, group = group)) +
  scale_fill_viridis_c(option = "A") +
  coord_fixed(1.3) + 
  theme_void() +
  transition_time(date) +
  labs(title = "Date: {frame_time}",
       fill = "ET (in)", x = "Lng", y = "Lat")
#anim_save("etb.gif", gifa, nframes = length(dtt), fps = 3) # save to root

# mercator projection in m and surface area of each cell
merc <- "+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
sa   <- prod(res(raster(spTransform(cv, merc), nrow = 100, ncol = 100)))
etdf <- tpsdf %>% 
  group_by(date) %>% 
  mutate(etv = et * 0.0254 * sa * 1e-9) %>% # convert inches to m and m3 to km3
  summarise(tot_et = sum(etv))              # km3


gifb <- ggplot(etdf, aes(date, tot_et)) + 
  geom_line() + 
  geom_point() + 
  theme_minimal() + 
  labs(x = "Date", y = expression(paste("Total ET (  ", km ^ 3, "  )")),
       title = "Total Daily ET in the Central Valley") + 
  transition_reveal(date)


a_gif <- animate(gifa, width = 240, height = 240)
b_gif <- animate(gifb, width = 360, height = 240)

library(magick)

a_mgif <- image_read(a_gif)
b_mgif <- image_read(b_gif)

new_gif <- image_append(c(a_mgif[1], b_mgif[1]))
for(i in 2:100){
  combined <- image_append(c(a_mgif[i], b_mgif[i]))
  new_gif  <- c(new_gif, combined)
}

new_gif
anim_save("etcomb.gif", new_gif) # save to root

