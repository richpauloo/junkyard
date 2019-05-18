####################################################################################################
# preprocess LBNL climate change data for TB and input to c2vsim model
####################################################################################################

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



####################################################################################################
# load data, subset to tulare basin, sanity checks to make sure data is correct
####################################################################################################

# load in all csv files
fp <- list.files("F:/Box Sync/Research/Post QE Research/work_by_person/zhilin/lbnl_clim/TB_CSV", full = TRUE)[-1]


# read all data into a list, fill missing lat/lon, convert dates
l <- vector("list", length = length(fp))
for(i in 1:length(l)){
  l[[i]] <- readr::read_csv(fp[[i]])
}

l <- do.call(rbind.data.frame, l)
l <- tidyr::fill(l, LAT) %>% tidyr::fill(LONG)



# convert to spatial data
coords <- as.matrix(l[, c("LONG", "LAT")])
spdf <- SpatialPointsDataFrame(coords = coords, 
                               data   = l, 
                               proj4string = CRS("+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))

# calcualte all ET
spdf@data <- spdf@data %>% mutate(ALLET = QSOIL + QVEGE + QVEGT)

# C2VSim elements shapefile:
#s <- shapefile("F:/Box Sync/Research/Post QE Research/work_by_person/zhilin/lbnl_clim/c2vsim/C2Vsim_mesh.shp")

# c2vSim FG
s <- shapefile("F:/Box Sync/Research/Post QE Research/work_by_person/zhilin/lbnl_clim/c2vsim/C2VSimFG_Elements.shp")

# plot elements and points
s <- spTransform(s, CRS("+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))

# plot(s)
# points(spdf, col = "red", pch = 16)

# subset to tulare basin: subbasins 14:21
# tb <- s[s@data$ISGE %in% 14:21, ]
# tb@data <- tb@data %>% dplyr::select(IE, ISGE) #only keep eleemnt id and sb id
# rownames(tb@data) <- NULL # remove rownames

tb <- s[s@data$SubRegion %in% 14:21, ]
tb@data <- tb@data %>% dplyr::select(ElementID, SubRegion) #only keep eleemnt id and sb id
rownames(tb@data) <- NULL # remove rownames

# plot(tb)
# points(spdf, col = "red", pch = 16)


# visualize first 12 months of precipt
spdf@data %>% 
  dplyr::filter(TIME %in% 197001:197012) %>% 
  ggplot(aes(LONG, LAT, fill = PRECT)) +
  geom_raster() + 
  theme_minimal() +
  scale_fill_viridis_c() +
  facet_wrap(~TIME)
  

####################################################################################################
# thin plate spline to interpolate points, return rasters for each var at each timestep
####################################################################################################

# interpolate each set of points 
# first make a list of dataframes: each element is a unique time
int <- split(spdf@data, f = spdf@data$TIME)

library(velox)
interp <- function(df, var, extr){
  tps <- Tps(df[, c("LONG", "LAT")], df[, var])
  p   <- raster(tb, nrow = 1000, ncol = 1000) # res: 0.002008855, 0.002061255  (x, y)
  p   <- interpolate(p, tps)
  # p   <- mask(p, tb)
  if(extr == FALSE){
    return(p)
  } 
  if(extr == TRUE){
    p   <- velox(p)
    rb  <- p$extract(tb, small = TRUE, fun = mean, df = TRUE)
    # rb  <- raster::extract(p, tb, sp = TRUE, fun = mean) # extract to tb elements
    #rb  <- rb@data
    #tb$var <- rb; return(tb) # sanity check for FG extraction
    return(rb)
  }   
}

# variables to get
vars <- colnames(spdf@data)[4:9]

# allocate
prect <- vector("list", length = length(unique(spdf@data$TIME)))
qover <- qsoil <- qvege <- qvegt <- allet <- prect
 
st <- Sys.time()
# calculate rasters
for(i in 1:length(prect)){
  prect[[i]] <- interp(int[[i]], vars[1], TRUE)
  qover[[i]] <- interp(int[[i]], vars[2], TRUE)
  qsoil[[i]] <- interp(int[[i]], vars[3], TRUE)
  qvege[[i]] <- interp(int[[i]], vars[4], TRUE)
  qvegt[[i]] <- interp(int[[i]], vars[5], TRUE)
  allet[[i]] <- interp(int[[i]], vars[6], TRUE)
}
#Sys.time() - st # took about 45 minutes

# add time
for(i in 1:length(prect)){
  prect[[i]]$time <- names(int)[i]
  qover[[i]]$time <- names(int)[i]
  qsoil[[i]]$time <- names(int)[i]
  qvege[[i]]$time <- names(int)[i]
  qvegt[[i]]$time <- names(int)[i]
  allet[[i]]$time <- names(int)[i]
}

# save
# readr::write_rds(prect, "C:/Users/rpauloo/Desktop/zhilin/tb/results/FG_prect.rds") 
# readr::write_rds(qover, "C:/Users/rpauloo/Desktop/zhilin/tb/results/FG_qover.rds") 
# readr::write_rds(qsoil, "C:/Users/rpauloo/Desktop/zhilin/tb/results/FG_qsoil.rds") 
# readr::write_rds(qvege, "C:/Users/rpauloo/Desktop/zhilin/tb/results/FG_qvege.rds") 
# readr::write_rds(qvegt, "C:/Users/rpauloo/Desktop/zhilin/tb/results/FG_qvegt.rds") 
# readr::write_rds(allet, "C:/Users/rpauloo/Desktop/zhilin/tb/results/FG_allet.rds") 
readr::write_rds(prect, "F:/Box Sync/Research/Post QE Research/work_by_person/zhilin/lbnl_clim/results/FG_prect.rds")
readr::write_rds(qover, "F:/Box Sync/Research/Post QE Research/work_by_person/zhilin/lbnl_clim/results/FG_qover.rds")
readr::write_rds(qsoil, "F:/Box Sync/Research/Post QE Research/work_by_person/zhilin/lbnl_clim/results/FG_qsoil.rds")
readr::write_rds(qvege, "F:/Box Sync/Research/Post QE Research/work_by_person/zhilin/lbnl_clim/results/FG_qvege.rds")
readr::write_rds(qvegt, "F:/Box Sync/Research/Post QE Research/work_by_person/zhilin/lbnl_clim/results/FG_qvegt.rds")
readr::write_rds(allet, "F:/Box Sync/Research/Post QE Research/work_by_person/zhilin/lbnl_clim/results/FG_allet.rds")

Sys.time() - st # took about 7.5 hours

####################################################################################################
# compile into a single file 
####################################################################################################

v1 <- bind_rows(prect) %>% rename(PRECT = out, TIME = time, ELEMENT_ID = ID_sp) %>% select(TIME, everything()) 
v2 <- bind_rows(qover) %>% select(out) %>% rename(QOVER = out) 
v3 <- bind_rows(qsoil) %>% select(out) %>% rename(QSOIL = out) 
v4 <- bind_rows(qvege) %>% select(out) %>% rename(QVEGE = out) 
v5 <- bind_rows(qvegt) %>% select(out) %>% rename(QVEGT = out) 
v6 <- bind_rows(allet) %>% select(out) %>% rename(ALLET = out) 

vf <- bind_cols(v1, v2, v3, v4, v5, v6)
rm(v1, v2, v3, v4, v5, v6)

# save
readr::write_csv(vf, "F:/Box Sync/Research/Post QE Research/work_by_person/zhilin/lbnl_clim/results/FG_lbnl_clim_c2vsim.csv")





####################################################################################################
# extract rasters to c2vsim model ploygons and save as csv
####################################################################################################

# allocate
prect_c2vsim <- vector("list", length = length(unique(spdf@data$TIME)))
qover_c2vsim <- qsoil_c2vsim <- qvege_c2vsim <- qvegt_c2vsim <- allet_c2vsim <- prect_c2vsim

st <- Sys.time()
# calculate rasters
for(i in 1:length(prect)){
  prect_c2vsim[[i]] <- interp(int[[i]], vars[1], TRUE)
  qover_c2vsim[[i]] <- interp(int[[i]], vars[2], TRUE)
  qsoil_c2vsim[[i]] <- interp(int[[i]], vars[3], TRUE)
  qvege_c2vsim[[i]] <- interp(int[[i]], vars[4], TRUE)
  qvegt_c2vsim[[i]] <- interp(int[[i]], vars[5], TRUE)
  allet_c2vsim[[i]] <- interp(int[[i]], vars[6], TRUE)
}
Sys.time() - st # took about 9 hours

# add time
for(i in 1:length(prect)){
  prect_c2vsim[[i]]$time <- names(int)[i]
  qover_c2vsim[[i]]$time <- names(int)[i]
  qsoil_c2vsim[[i]]$time <- names(int)[i]
  qvege_c2vsim[[i]]$time <- names(int)[i]
  qvegt_c2vsim[[i]]$time <- names(int)[i]
  allet_c2vsim[[i]]$time <- names(int)[i]
}

# save
readr::write_rds(prect_c2vsim, "C:/Users/rpauloo/Desktop/zhilin/tb/results/prect_c2vsim.rds") 
readr::write_rds(qover_c2vsim, "C:/Users/rpauloo/Desktop/zhilin/tb/results/qover_c2vsim.rds") 
readr::write_rds(qsoil_c2vsim, "C:/Users/rpauloo/Desktop/zhilin/tb/results/qsoil_c2vsim.rds") 
readr::write_rds(qvege_c2vsim, "C:/Users/rpauloo/Desktop/zhilin/tb/results/qvege_c2vsim.rds") 
readr::write_rds(qvegt_c2vsim, "C:/Users/rpauloo/Desktop/zhilin/tb/results/qvegt_c2vsim.rds") 
readr::write_rds(allet_c2vsim, "C:/Users/rpauloo/Desktop/zhilin/tb/results/allet_c2vsim.rds") 


####################################################################################################
# compile into a single file 
####################################################################################################

v1 <- bind_rows(prect_c2vsim) %>% rename(PRECT = layer, TIME = time) %>% select(TIME, everything()) 
v2 <- bind_rows(qover_c2vsim) %>% select(layer) %>% rename(QOVER = layer) 
v3 <- bind_rows(qsoil_c2vsim) %>% select(layer) %>% rename(QSOIL = layer) 
v4 <- bind_rows(qvege_c2vsim) %>% select(layer) %>% rename(QVEGE = layer) 
v5 <- bind_rows(qvegt_c2vsim) %>% select(layer) %>% rename(QVEGT = layer) 
v6 <- bind_rows(allet_c2vsim) %>% select(layer) %>% rename(ALLET = layer) 

vf <- bind_cols(v1, v2, v3, v4, v5, v6)
rm(v1, v2, v3, v4, v5, v6)

# save
readr::write_csv(vf, "C:/Users/rpauloo/Desktop/zhilin/tb/results/lbnl_clim_c2vsim.csv")

  


