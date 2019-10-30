# create 3d line
# data.frame with columns x,y,z
path <- data.frame(x = c(245, 233, 270, 400, 380)-200,
                   y = c(245, 270, 138, 225, 300)-100,
                   z = c(0, 1.2, 5, 3, 9))


plot3D::scatter3D(path$x, path$y, path$z, type = "b", bty = "g", 
                  phi = 0, col = "red", pch = 20, ticktype = "detailed")

# create 3d matrix of element centroids
nx = 4
ny = 4
nz = 8
dx = 50
dy = 50 
dz = 1

# vectors of matrix centroids in x,y,z, named by index
cx <- seq(0.5*dx, (nx * dx), dx)
cy <- seq(0.5*dy, (ny * dy), dy)
cz <- seq(0.5*dz, (nz * dz), dz)
names(cx) <- 1:nx
names(cy) <- 1:ny
names(cz) <- 1:nz

# data frame of x,y,z,hf where
# x,y,z are row, column, layer, and hf is hydrofacies
set.seed(1)
hf_key <- expand.grid(list(x = 1:nx, y = 1:ny, z = 1:nz))
hf_key$hf <- sample(1:5, nrow(hf_key), replace = TRUE)

# step 1: determine row, column, layer (x,y,z) of points in path
closest_centroid <- function(centroids, vec){
  as.numeric(names(centroids)[which.min(abs(vec-centroids))])
}

ix <- sapply(path$x, closest_centroid, centroids = cx)
iy <- sapply(path$y, closest_centroid, centroids = cy)
iz <- sapply(path$z, closest_centroid, centroids = cz)


# step 4: lookup hf and add to the path dataframe
path_hf <- sapply(1:nrow(path), function(j) {
    pull(filter(hf_key, x == ix[j], y == iy[j], z == iz[j]), hf)
  }
)

path$hf <- path_hf

# due to the nature of the interpolation, we add more points
# than we need along each particle path. We leave these in 
# to calculate and plot the facies residence length (FRL) 
# because it improves the resolution of the path, and thus, the
# places where the paths breaks between hydrofacies. 
# now we can recycle utility functions to calculate the arc of
# each segment, assign arc lengths to each segment, and count the
# FRL


# iterate over all particle paths
