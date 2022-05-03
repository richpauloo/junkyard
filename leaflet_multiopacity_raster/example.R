library(leaflet)
library(leaflet.multiopacity)
library(raster)

# Create raster example
r <- raster(xmn = -2.8, xmx = -2.79,
            ymn = 54.04, ymx = 54.05,
            nrows = 30, ncols = 30)
values(r) <- matrix(1:900, nrow(r), ncol(r), byrow = TRUE)
crs(r) <- CRS("+init=epsg:4326")

leaflet() %>%
  addProviderTiles("OpenStreetMap", layerId = "base") %>%
  addRasterImage(r, layerId = "r1") %>%
  addRasterImage(r, layerId = "r2") %>%
  addOpacityControls(layerId = c("r1", "r2"))
