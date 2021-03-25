# ggplot options
library(ggplot2)

# base theme that enables markdown text in common locations
rich_theme <- theme_minimal(base_size = 12, base_family = "Open Sans") +
  theme(
    axis.ticks        = element_line(color = "grey92"),
    axis.ticks.length = unit(.5, "lines"),
    panel.grid.minor  = element_blank(),
    plot.title    = ggtext::element_markdown(),
    plot.subtitle = ggtext::element_markdown(),
    plot.caption  = ggtext::element_markdown(),
    axis.title.x  = ggtext::element_markdown(),
    axis.title.y  = ggtext::element_markdown(),
    legend.title  = ggtext::element_markdown(),
    plot.title.position   = 'plot',
    plot.caption.position = 'plot'
  ) 

# add color guide to top of plot and expand width
# use with theme(legend.position = "top")
rich_guide_top <- guides(color = guide_colorbar(title.position = 'top', 
                                                title.hjust = .5,
                                                barwidth = unit(20, 'lines'), 
                                                barheight = unit(.5, 'lines'))) 

# non-expanded or clipped coords
rich_coords <- coord_cartesian(expand = FALSE, clip = "off")