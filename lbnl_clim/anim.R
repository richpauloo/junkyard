lmax <- sapply(prect, function(x) max(x@data@values, na.rm = T)) %>% sort() %>% .[443]
lmin <- 0

pd <- vector("list", length = length(prect))
for(i in 1:444){
  pd[[i]]      <- as.data.frame(prect[[i]], xy = TRUE)
  pd[[i]]$time <- names(int)[i]
}
pd <- bind_rows(pd) %>% filter(!is.na(layer))
pd$time <- as.numeric(paste0(pd$time, "01"))
pd$time <- lubridate::ymd(pd$time)
pd <- pd %>% mutate(layer = ifelse(layer > lmax, lmax, layer))

library(gganimate)
st <- Sys.time()
gga <- ggplot(pd, aes(x, y, fill = layer)) +
  geom_raster() + 
  theme_minimal() + 
  scale_fill_viridis_c() +
  coord_fixed(1.3) +
  transition_time(time) +
  labs(title = "Time: {frame_time}",
       fill = "PRECT", x = "LON", y = "LAT")
anim_save("precip.gif", gga, nframes = 444, fps = 12) # save to root
Sys.time() - st