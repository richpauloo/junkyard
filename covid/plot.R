library(tidyverse)
library(raster)
library(mapview)
library(sp)
library(leaflet)

d  <- raster("~/Desktop/covid/COVID_environmental_impacts/Seoul19.tif")
d2 <- raster("~/Desktop/covid/COVID_environmental_impacts/Seoul20.tif")

d  <- as.data.frame(d, xy = TRUE) %>% rename(b = Seoul19)
d2 <- as.data.frame(d2, xy = TRUE) %>% rename(b = Seoul20)

d$b %>% hist()
d2$b %>% hist()

x <- quantile(d$b, 0.99)

#x2 <- quantile(d2$b, 0.99)
# d$b[d$b >= quantile(d$b, 0.97)] <- quantile(d$b, 0.97)
# d2$b[d2$b >= quantile(d2$b, 0.97)] <- quantile(d2$b, 0.97)

d$b[d$b >= x] <- x
d2$b[d2$b >= x] <- x

d$t <- "February 2019"
d2$t <- "February 2020"

z <- bind_rows(d, d2)

p <- ggplot(z, aes(x,y,fill=scales::rescale(b, c(0,1)))) +
  geom_raster() +
  facet_wrap(~t) +
  theme_minimal() +
  scale_fill_viridis_c(option="A", 
                       guide = guide_colorbar(direction = "horizontal", 
                                              barwidth = 27, title.position = "top",
                                              title.hjust = 0.5)) +
  theme(legend.position = "bottom") +
  labs(fill = "Brightness",
       title = "NASA VIIRS Nighttime Radiance in Seoul, South Korea",
       x=NULL,y=NULL)

p

ggsave(p, filename = "~/Desktop/covid/p3.pdf", device = cairo_pdf, height = 4, width = 6)
