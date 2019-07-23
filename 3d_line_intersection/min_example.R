# create 3d line
# data.frame with columns x,y,z
path <- data.frame(x = c(245, 233, 270, 400, 380),
                   y = c(245, 270, 138, 225, 300),
                   z = c(0, 1.2, 5, 3, 9))


plot3D::scatter3D(path$x, path$y, path$z, type = "b", bty = "g", 
                  phi = 0, col = "red", pch = 20, ticktype = "detailed")

# create 3d matrix of element centroids
nx = 5
ny = 7
nz = 10
dx = 100
dy = 100 
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
hf_key

# step 1: determine row, column, layer (x,y,z) of points in path
closest_centroid <- function(centroids, vec){
  as.numeric(names(centroids)[which.min(abs(vec-centroids))])
}

ix <- sapply(path$x, closest_centroid, centroids = cx)
iy <- sapply(path$y, closest_centroid, centroids = cy)
iz <- sapply(path$z, closest_centroid, centroids = cz)


# step 4: lookup hf
pull(filter(hf_key, x == ix, y == iy, z == iz), hf)

# iterate over all particle paths
