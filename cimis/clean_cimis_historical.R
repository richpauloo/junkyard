##########################################################################
# Use all historical data to estimate total monthly Eto at CV scale
##########################################################################

library(raster)
library(sp)
library(fields)
library(tidyverse)

df <- readr::read_csv("C:/Users/rpauloo/Desktop/cimis_stations_spatial.csv")

# remove non-numeric entries from Eto column
test <- filter(df, !D %in% c("--", "*", "F", "H", "M", "P", "R", "S"))

# sanity check: FAILS
count(test, year)
rm(test)

# FIX ERROR: for all years <= 2013, columns D and E are switched
df1 <- filter(df, year <= 2013) %>% rename(D = E, E = D) %>% select(A:C, D, E, everything())
df2 <- filter(df, year >= 2014)
df3 <- bind_rows(df1, df2)

# now apply filter
df3 <- filter(df3, !D %in% c("--", "#####")) %>% 
  mutate(D = as.numeric(D))

# sanity check: passes
nrow(df3) == df3$D %>% as.numeric() %>% length()

# sanity check: passes
count(df3, year) %>% 
  ggplot(aes(year, n)) +
  geom_line()


##########################################################################
# split by year, average by month, TPS, and visualize
##########################################################################

l <- split(df3, df3$year)

l_mean <- vector("list", length = length(l))
for(i in 1:length(l)){
  # add months in year i
  l[[i]]$month <- l[[i]]$B %>% substr(., 1,2) %>% as.numeric()
  
  # for each month compute station means (can also do min and max later)
  l_mean[[i]] <- l[[i]] %>% 
    group_by(month, A, lat, lng, year) %>% 
    summarise(eto_mean = mean(D, na.rm = TRUE)) %>% 
    ungroup()
}

l_mean <- bind_rows(l_mean)

# only take months with at least 30 station means
rule <- l_mean %>% count(year, month)

l_mean <- left_join(l_mean, rule, by = c("year", "month")) %>% 
  filter(n >= 30)

# sanity check: PASS
l_mean %>% count(year, month) %>% arrange()

# interpolate Tps for each month
l_mean$ym <- paste0(l_mean$year, "-", formatC(l_mean$month, 
                                              width = 2, 
                                              flag = "0"))

# subset for month present and in CV
l_mean <- filter(l_mean, !is.na(month))

# load CVm make l_mean into spatial object, subset to stations in CV
ll <- "+init=epsg:4269 +proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs +towgs84=0,0,0"
cv <- raster::shapefile("C:/Users/rpauloo/Documents/Github/junkyard/cimis/data/Alluvial_Bnd.shp")
cv <- spTransform(cv, ll)

# make stations into spatial object
spdf <- SpatialPointsDataFrame(coords      = as.matrix(l_mean[,c("lng","lat")]), 
                               data        = l_mean, 
                               proj4string = CRS(ll))

# subset of points within cv
spdf <- spdf[cv, ]

# sanity check: PASS
plot(cv); points(spdf, pch = 16, col="red") # points plot in CV






##########################################################################
# RESUME HERE
##########################################################################


# sanity check: PASS
temp    <- split(spdf, spdf$ym)

sapply(temp, nrow) # observations per ym timestep

# seems like we're missing many months...










# only take 30 observations per timestep

# sanity check: continuous months?

tpsr <- vector("list", length = length(tps))
for(i in 1:length(tpsr)){
  tpsr[[i]] <- 
}




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










