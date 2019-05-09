# 1D ADE

library(VGAM)
library(tidyverse)

# Calculate linear pore velocity in z from 3D MODFLOW zy output
# "C:/Users/rpauloo/Documents/GitHub/kings-river-fan-tprogs/code/modflow/3d_model_zy_bc/gv83.list"
Q     = 31.6689 * 86400    # Steady state Flux [m^3/days]
A     = 12.6 * 15 * 1000^2 # Surface area [m^2]
n     = 0.3                # porosity of high K facies (could also represent upscaled porosity)


# Solve 1D ADE with the Ogata Banks method (1961) for continuous source
# https://pubs.usgs.gov/pp/0411a/report.pdf

# # params
C0    = 1
x     = 60
v     = Q / (A * n)        # linear velocty in z
alpha = 50                 # dispersivity in z [m]
D     = alpha * v          # dispersion coefficient in z  
vx_D  = v*x/D
evx_D = exp(vx_D)


time = 1:10000

# solve
ade <- function(c0, x, v, alpha, time){
  
  D     = alpha * v          
  vx_D  = v*x/D
  evx_D = exp(vx_D)
  
  df <- data.frame(t = time) %>% 
    mutate(xmvt = x - v*t,
           xpvt = x + v*t,
           tdts = 2*((D*t)^0.5),
           erfa = erf(xmvt / tdts),
           erfb = erf(xpvt / tdts),
           cdc0 = 0.5 * ((1-erfa) + evx_D*(1-erfb)) ) %>% 
    select(t, cdc0) %>% 
    mutate(alpha = alpha)
  
  return(df)
  
}
  

# solve across a grid of alpha
alphas <- c(0.5, 5, 50)

l <- vector("list", length = length(alpha))
for(i in 1:length(alphas)){
  l[[i]] <- ade(c0, x, v, alphas[i], time)
}

l <- bind_rows(l) %>% 
  mutate(alpha = factor(alpha),
         t = t/365) 

# plot
cbtc    <- read_rds("F:/Box Sync/Research/Post QE Research/ADE/cbtc_df.rds")
cbtc_3d <- read_rds("F:/Box Sync/Research/Post QE Research/ADE/cbtc_3d.rds")
cols <- structure(c(rep("gray50",63)),
                  names = c(paste0("slice_",1:63)))

p1 <- cbtc %>% 
  filter(t_yr <= 27.5) %>% 
  ggplot(aes(t_yr, density, color = slice_id)) + # 2d slices
  scale_color_manual(values = cols) + 
  geom_line(alpha = 0.5) +
  geom_line(data = filter(cbtc_3d, t_yr <= 27.5), aes(t_yr, density), color = "black") + # 3d slice
  guides(color = FALSE) +
  coord_cartesian(xlim = c(0, 25)) +
  labs(x = "Time (yrs)",
       y = expression(C/Co)) +
  theme_minimal()
  

p2 <- l %>% 
  ggplot(aes(t, cdc0, color = alpha)) +
  geom_line() +
  theme_minimal() +
  theme(legend.position = c(.9,.2)) +
  labs(color = expression(alpha),
       x = "Time (yrs)",
         y = expression(C/Co)) +
  coord_cartesian(xlim = c(0,25))


p3 <- cbtc %>% 
  filter(t_yr <= 27.5) %>% 
  ggplot(aes(t_yr, density, color = slice_id)) + # 2d slices
  scale_color_manual(values = cols) + 
  geom_line(alpha = 0.5) +
  geom_line(data = filter(cbtc_3d, t_yr <= 27.5), aes(t_yr, density), 
            color = "black", size = 1) + # 3d slice
  geom_line(data = rename(filter(l, alpha == 0.5), t_yr = t, density = cdc0), 
            aes(t_yr, density), col = "#F8766D", size = 1) +
  geom_line(data = rename(filter(l, alpha == 5), t_yr = t, density = cdc0), 
            aes(t_yr, density), col = "#7CAE00", size = 1) +
  geom_line(data = rename(filter(l, alpha == 50), t_yr = t, density = cdc0), 
            aes(t_yr, density), col = "#00BFC4", size = 1) +
  guides(color = FALSE) +
  coord_cartesian(xlim = c(0, 25)) +
  labs(x = "Time (yrs)",
       y = expression(C/Co)) +
  theme_minimal()
  

p4 <- cowplot::plot_grid(p1, p2, ncol = 2, labels = c("A","B"))  
p3 <- cowplot::plot_grid(p3, labels = "C")
p5 <- cowplot::plot_grid(p4, p3, nrow = 2)
ggsave(p5, filename="p5.png", dpi = 300, height = 7, width = 7)
