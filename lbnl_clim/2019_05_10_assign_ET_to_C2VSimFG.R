##########################################################################
# preprocess LBNL climate change data for TB and input to c2vsim model
##########################################################################

# packages used
library(sp)
library(raster)
library(dplyr)
library(lubridate)
library(tidyr)
library(readr)
library(maptools)
library(ggplot2)
library(fields)



########################################################################## 
# load data, subset to tulare basin, sanity checks to make sure data is correct
##########################################################################

# load in all csv files
fp <- list.files("F:/Box Sync/Research/Post QE Research/work_by_person/zhilin/lbnl_clim/TB_CSV_2019_05_10", full = TRUE, pattern = ".csv$")


# read all data into a list, fill missing lat/lon, convert dates
l <- vector("list", length = length(fp))
for(i in 1:length(l)){
  l[[i]] <- xlsx::read.xlsx(fp[[i]], 
                            sheetIndex = 1,
                            header = FALSE)
  l[[i]] <- l[[i]] %>% 
    rename(TIME = X1, LAT = X2, 
           LONG = X3, ET = X14) %>% 
    mutate(LAT  = ifelse(LAT  == 0, NA, LAT ),
           LONG = ifelse(LONG == 0, NA, LONG)) %>% 
    tidyr::fill(LAT, LONG) %>% 
    select(TIME, LAT, LONG, ET)
}

l <- do.call(rbind.data.frame, l)

# convert to spatial data
coords <- as.matrix(l[, c("LONG", "LAT")])
spdf <- SpatialPointsDataFrame(coords = coords, 
                               data   = l, 
                               proj4string = CRS("+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))

# c2vSim FG
s <- shapefile("F:/Box Sync/Research/Post QE Research/work_by_person/zhilin/lbnl_clim/c2vsim/C2VSimFG_Elements.shp")

# plot elements and points
s <- spTransform(s, CRS("+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))

# plot(s)
# points(spdf, col = "red", pch = 16)

# subset to tulare basin: subbasins 14:21
tb <- s[s@data$SubRegion %in% 14:21, ]
tb@data <- tb@data %>% dplyr::select(ElementID, SubRegion) # only keep element and sb id
rownames(tb@data) <- NULL # remove rownames

# plot(tb)
# points(spdf, col = "red", pch = 16)


# visualize first 12 months of precipt
spdf@data %>% 
  dplyr::filter(TIME %in% 197001:197012) %>% 
  ggplot(aes(LONG, LAT, fill = log(ET))) +
  geom_raster() + 
  theme_minimal() +
  scale_fill_viridis_c() +
  facet_wrap(~TIME)


##########################################################################
# thin plate spline to interpolate points, return rasters for each var at each timestep
##########################################################################

# interpolate each set of points 
# first make a list of dataframes: each element is a unique time
int <- split(spdf@data, f = spdf@data$TIME)

library(velox)
interp <- function(df, var, extr){
  tps <- Tps(df[, c("LONG", "LAT")], df[, var])
  p   <- raster(tb, nrow = 1000, ncol = 1000) # res: 0.002008855, 0.002061255  (x, y)
  p   <- interpolate(p, tps)

  if(extr == FALSE){
    return(p)
  } 
  
  if(extr == TRUE){
    p   <- velox(p)
    rb  <- p$extract(tb, small = TRUE, fun = mean, df = TRUE)
    return(rb)
  }   
  
}

# variables to get
var <- "ET"

# allocate
allet <- vector("list", length = length(unique(spdf@data$TIME)))

st <- Sys.time()
# calculate rasters
for(i in 1:length(allet)){
  allet[[i]] <- interp(int[[i]], var, TRUE)
}
Sys.time() - st # took about 45 mins

# add time
for(i in 1:length(allet)){
  allet[[i]]$time <- names(int)[i]
}

# save
readr::write_rds(allet, "F:/Box Sync/Research/Post QE Research/work_by_person/zhilin/lbnl_clim/results/2019_15_10_FG_allet.rds")


####################################################################################################
# compile into a single file 
####################################################################################################

v <- bind_rows(allet) %>% rename(ET = out, TIME = time) %>% select(TIME, everything())

# save
readr::write_csv(v, "F:/Box Sync/Research/Post QE Research/work_by_person/zhilin/lbnl_clim/results/2019_15_10_FG_lbnl_clim_c2vsim.csv")




