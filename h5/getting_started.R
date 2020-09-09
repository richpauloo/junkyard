library(raster)

# path to one of the .h5 files
# from ftp://ftp.snow.ucsb.edu/pub/org/snow/products/ParBal/Sierra/
f <- file.path("~","Downloads","reconstruction_Sierra_2019.h5")

# install necessary packages
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install(version = "3.11")

# type a for all
# type no
BiocManager::install("rhdf5")
# type a for all
# type no

library(rhdf5)

# sanity check
x <- h5ls(f)

# read data
d <- h5read(f, "/Grid/swe")

# view data structure
str(d)
class(d) 
# it's an array, which is a rarely used R data structure with 
# nrow x ncol x nlayer. It's essentially a stack of matrices.

# index arrays with row, column, layer indexing array[r,c,l].
# for example, select the element in the first row, column, layer
d[1,1,1]

# now select all rows and columns in layer 1, which is a matrix
layer1 <- d[,,1]

# sanity check that it's a matrix
class(layer1)
ncol(layer1)
nrow(layer1)

# this is spaptial data, so let's get it into a spatial object and 
# plot this sucker!
r <- raster::raster(layer1)
sp::spplot(r)

# that looks terrible
# closer inspection of the readme.txt shows that the "FillValue"
# is 65535. Let's replace these with NA, since R is reading these
# as real values.
d[which(d == 65535)] <- NA

# again subset for layer 1
layer1 <- d[,,1]
r <- raster::raster(layer1)
sp::spplot(r)

# that looks better, but also terrible
# it looks like units are in mm, and most cells have a value of 0
sort(table(r@data@values))

# what if we're just in a part of the year with no snow?
layer1 <- d[,,100]
r <- raster::raster(layer1)
sp::spplot(r)

# that's more like it.
