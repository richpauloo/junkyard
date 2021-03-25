## change global theme settings (for all following plots)
library(tidyverse)

mtcars %>% 
  ggplot(aes(mpg, rownames(mtcars), color = rownames(mtcars))) +
  geom_point() +
  labs(title = "A **cool** plot",
       subtitle = "A <b style='color:#FF0000';>asdf</b> **subtitle**", 
       color = "**MPG** (ft/v)",
       x = "*mpg* (ft/c)") +
  # rcartocolor::scale_color_carto_d(palette = "Safe", guide = "none") +
  scico::scale_color_scico_d(palette = "romaO", direction = -1) +
  # theme_minimal(base_size = 12, base_family = "Open Sans") +
  rich_theme +
  theme(legend.position = "top") + 
  rich_coords +
  guides(color = guide_colorsteps(title.position = 'top', 
                                title.hjust = .5,
                                barwidth = unit(20, 'lines'), 
                                barheight = unit(.5, 'lines'))) 


library(ggforce)
library(ggfx)
ggplot() +
  with_blur(geom_point(data = mtcars, 
                       aes(mpg, disp, color = mpg, size = mpg+2)), 
            sigma = 3) +
  geom_point(data = mtcars, aes(mpg, disp, color = mpg, size = mpg)) +
  ggforce::geom_link(
    data = mtcars,
    aes(
      x = mpg, 
      xend = mpg,
      y = 0,
      yend = disp,
      color = mpg,
      color = after_scale(colorspace::desaturate(color, .3)),
      alpha = mpg
    ),
    n = 300,
    size = 0.6
  ) +
  scico::scale_colour_scico(palette = "hawaii") +
  theme_void() +
  theme(plot.background = element_rect(fill = "black"), 
        legend.text = element_text(color = "white"),
        legend.title = element_text(color = "white")) +
  guides(size = FALSE) +
  coord_polar(theta = "y", start = 4.71, clip = "off")
