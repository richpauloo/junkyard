system.file("tif/L7_ETMs.tif", package = "stars") %>% read_stars() -> x
library(ggplot2)

ggplot() + geom_stars(data = x) +
  coord_equal() +
  facet_wrap(~band) +
  theme_void() +
  scale_x_discrete(expand=c(0,0))+
  scale_y_discrete(expand=c(0,0))



# get 
library(raster)
d <- getData('alt', country='USA', mask=TRUE)
ca <- shapefile("F:/Box Sync/water_data_challenge/data/spatial/ca_boundary/CA_State_TIGER2016.shp")

crs(d[[1]])
crs(ca)

library(sp)
ca <- spTransform(ca, crs(d[[1]]))

car <- crop(d[[1]], ca)
car <- mask(car, ca)

carm <- as.matrix(car)
carm[is.na(carm)] <- 0
  
carm %>% 
  sphere_shade() %>% 
  add_shadow(ray_shade(carm)) %>% 
  plot_3d(carm, zscale = 100)
  
  
  
  
  