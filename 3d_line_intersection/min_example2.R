library(dplyr)
library(purrr)
library(plotly)

path3d <- data.frame(
  x = c(245, 233, 270, 400, 380),
  y = c(245, 270, 138, 225, 300),
  z = c(0, 1.2, 5, 3, 9),
  t = 1:5)


path3d_interp_funs <- 
  map(path3d, ~approxfun(path3d$t, .x))

root_finder <- function(f, zero, range, cuts) {
  
  endpts <- seq(range[1], range[2], length.out = cuts+1)
  
  range_list <- map2(endpts[-(cuts+1)], endpts[-1], c)
  
  safe_root <- possibly(uniroot, otherwise = NULL)
  f0 <- function(x) f(x) - zero
  
  map(range_list, ~safe_root(f0, interval = .x, maxiter = 100)$root) %>% 
    compact() %>% 
    unlist() %>% 
    unique()
  
}

root_finder(path3d_interp_funs$z, zero = 4, range = range(path3d$t), cuts = 10)

t_solutions <- map(
  1:8, 
  ~root_finder(path3d_interp_funs$z, zero = .x, range = range(path3d$t), cuts = 100)
) %>% unlist()

path <- map(path3d_interp_funs, ~.x(t_solutions)) %>% 
  as_tibble()

path <- arrange(path, t)

plot3D::scatter3D(path$x, path$y, path$z, type = "b", bty = "g", 
                  phi = 0, col = "red", pch = 20, ticktype = "detailed")
