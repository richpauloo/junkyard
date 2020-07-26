# ------------------------------------------------------------------------
# ndwi for an image. NDWI for water body detectin is the green to NIR 
# normalized difference (McFeeters, 1996)
# ------------------------------------------------------------------------

library(rgee)

# Initialize Earth Engine
# ee_clean_credentials()
ee_Initialize()

# for Landsat, NDWI is the nd of b2 and b4
ndwi_landsat <- function(image) {
  return(image$normalizedDifference(c("B2", "B4")))
}

# for Sentinel, NDWI is the nd of b3 and b8
ndwi_sentinel <- function(image) {
  return(image$normalizedDifference(c("B3", "B8")))
}

# ------------------------------------------------------------------------
# on a single image
# ------------------------------------------------------------------------
# landsat collection
x <- ee$Image("LANDSAT/LT05/C01/T1_TOA/LT05_044034_20100611")
ndwi <- ndwi_landsat(x)

# Define visualization parameters in an object literal.
vizParams <- list(
  #gamma = 1.3,
  min = -1, 
  max = 1,
  palette = rev(colormap::colormap(colormap::colormaps$viridis, nshades=8))
)

Map$centerObject(ndwi)
Map$addLayer(ndwi, vizParams, "Landsat NDWI") 

# ------------------------------------------------------------------------
# Landsat ndwi for an imageCollection
# ------------------------------------------------------------------------

# california
featureCollection <- ee$FeatureCollection("TIGER/2016/States")
ca <- featureCollection$filter(ee$Filter$eq("NAME", "California"))

# images
x  <- 
  ee$ImageCollection("LANDSAT/LC08/C01/T1_TOA")$
  filterDate('2017-08-01', '2017-08-30')$
  median()$
  clipToCollection(ca)

ndwi_2 <- ndwi_landsat(x)

Map$addLayer(ndwi_2, vizParams, "Landsat NDWI") 

# ------------------------------------------------------------------------
# Sentinel ndwi for an imageCollection
# ------------------------------------------------------------------------

# images
y  <- 
  ee$ImageCollection("COPERNICUS/S2")$
  filterDate('2017-01-01', '2017-12-31')$
  filterBounds(ca)

# Create a number ee$List where each element represent a month
months <- ee$List$sequence(1, 12)

# Function to calculate a monthly composite
monthly_median <- function(m) {
  y$filter(ee$Filter$calendarRange(m, m, "month")) %>%
    ee$ImageCollection$median() %>%
    ee$Image$select("B3", "B8")
}

y_monthly <- months$map(ee_utils_pyfunc(monthly_median))

# function to map ndwi over imageCollection
add_ndwi = function(image) {
  ndwi = image$normalizedDifference(c('B5', 'B4'))$rename('NDWI')
  return(image$addBands(ndwi))
}

# Define visualization parameters in an object literal.
vizParams <- list(
  gamma = 1.3,
  min = -1, 
  max = 1
)

# test on a single image
Map$addLayer(add_ndwi(y$first())$select("NDWI"), vizParams)

# map over imageCollection
y_monthly_ndwi <- y_monthly$map(add_ndwi)



y_monthly_ndwi <- y_monthly$map(ee_utils_pyfunc(ndwi_sentinel))

Map$addLayer(ndwi_2, vizParams, "Sentinel NDWI")


# ------------------------------------------------------------------------
# JRC - monthly water recurrence - long term averages
# https://developers.google.com/earth-engine/datasets/catalog/JRC_GSW1_2_MonthlyRecurrence#image-properties
# ------------------------------------------------------------------------

# images
z  <- 
  ee$ImageCollection("JRC/GSW1_2/MonthlyRecurrence")$
  #filterDate("2000-05-01", "2007-12-01")$
  #filterBounds(ca)
  #filterDate('2017-01-01', '2017-03-01')$
  #median()$
  filterBounds(ca)
#ee_print(z)

# Get the number of images.
z$size()$getInfo()

# get information on the first image
z$first()$getInfo()

# object class?
z$name()
z$first()$name()

# Get the date range of images in the collection.

z1 <- z$first()

# Define visualization parameters in an object literal.
vizParams <- list(
  bands = "monthly_recurrence",
  min = 0, 
  max = 100,
  palette = c('ffffff', 'ffbbbb', '0000ff')
)

Map$addLayer(z1, vizParams, "JRC Monthly water recurrence")


# ------------------------------------------------------------------------
# 
# ------------------------------------------------------------------------
