library(tidyverse)
library(here)
library(sf)
library(ggmap)

# preprocessed data
d <- read_rds(here("data_output/preprocessed.rds")) %>% 
  mutate(
    rd = lubridate::mdy_hm(ReportDate),
    year = lubridate::year(rd)
  ) 

# theme
theme_md <- 
  theme_void() + 
  theme(
    axis.line.x = element_line(color = "grey40"), 
    axis.text.x = element_text(color = "grey40"), 
    axis.ticks.x = element_line(color = "grey40"), 
    axis.ticks.length.x = unit(.4, "lines"), 
    plot.margin = margin(10, 40, 10, 20),
    plot.title = ggtext::element_markdown(),
    plot.subtitle = ggtext::element_markdown(),
    plot.caption = ggtext::element_markdown(),
    axis.title.x = ggtext::element_markdown(),
    axis.title.y = ggtext::element_markdown()
  )


# burg cols ---------------------------------------------------------------
# primary violation
d2 <- d %>% 
  st_drop_geometry() %>% 
  mutate(pvf = fct_lump(PrimaryViolation, 10)) %>% 
  count(pvf) %>% 
  mutate(color = ifelse(pvf == "PC 459 Burglary", "#28A87D", "grey50")) %>% 
  filter(!is.na(pvf) & pvf != "Other") 

# burglary plot
p <- d2 %>% 
  ggplot(aes(n, fct_reorder(pvf, n))) + 
  geom_text(aes(label = glue::glue("     {pvf}")), nudge_x = 1, hjust = 0) +
  geom_segment(aes(x = 0, y = pvf, xend = n, yend = pvf), color = "grey70") +
  geom_point(fill = d2$color, aes(size = n), pch = 21) +
  labs(x = "", y = "",
       title = "**<i style='color:#28A87D;'>Burglary</i>** 
                is the most common crime in Sacramento County",
       subtitle = "_Years: 2007-2021_") +
  theme_md +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  coord_cartesian(xlim = c(0, 100000), clip = "off", expand = FALSE) +
  expand_limits(y = c(11, -.03)) +
  scale_x_continuous(label = scales::comma) +
  guides(size = "none") 
p


# site map ----------------------------------------------------------------
# Sac county spatial data
sac <- USAboundaries::us_counties(states = "CA", resolution = "high") %>% 
  filter(name == "Sacramento") %>% 
  st_transform(3310)

# map 
m <- filter(d, PrimaryViolation == "PC 459 Burglary")
m2020 <- m %>% filter(year == 2020) %>% st_transform(4269)

# site map
bm <- get_stamenmap(bbox = st_bbox(st_transform(st_buffer(st_as_sfc(st_bbox(sac)), 2000), 4269)) %>% 
                      setNames(c("left","bottom","right","top")),
                    zoom = 11, source = "stamen", maptype = "toner-hybrid")

# plot
p2 <- ggmap(bm) + 
  geom_sf(data = st_transform(sac, 4269), inherit.aes = FALSE, fill = NA, color = "grey30", lwd = 1) +
  geom_sf(data = m2020, inherit.aes = FALSE, fill = "#28A87D", pch = 21, alpha = 0.4) +
  scale_fill_viridis_c() +
  theme_void() +
  theme(plot.title = ggtext::element_markdown()) +
  labs(title = "Most **<i style='color:#28A87D;'>burglaries</i>** occur in _**suburban areas**_",
       subtitle = "")
p2


# waffle plot -------------------------------------------------------------
# crime over time
gh_waffle <- function(data) {
  p <- ggplot(data, aes(x = week, y = day, fill = n)) +
    scale_fill_gradient(
      name = "",
      low = "white",
      high = "#28A87D",
      na.value = "grey90",
      limits = c(0, max(data$n))
    ) +
    geom_tile(color = "white", size = 0.2) +
    facet_wrap("year", ncol = 3) +
    scale_x_continuous(
      expand = c(0, 0),
      breaks = seq(1, 52, length = 12),
      labels = c(
        "Jan", "Feb", "Mar", "Apr", "May", "Jun",
        "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
      )
    ) +
    theme(
      axis.title = element_blank(),
      axis.ticks = element_blank(), 
      axis.text.y = element_text(),
      panel.grid = element_blank(),
      aspect.ratio = 1/6,
      legend.key.width = unit(1, "cm"),
      strip.text = element_text(hjust = 0.00, face = "bold"),
      legend.position = "top", 
      panel.spacing.x = unit(1, "lines"),
      panel.spacing.y = unit(0.5, "lines"),
      plot.title = ggtext::element_markdown(),
      plot.subtitle = ggtext::element_markdown()
    ) +
    guides(fill = guide_colorbar(barwidth = unit(20, "lines"), 
                                  barheight = unit(1, "lines"),
                                  title.hjust = 0.5,
                                  title.vjust = 0.8))
  
  print(p)
}

# burglaries over time
p3 <- m %>% 
  st_drop_geometry() %>% 
  filter(year %in% 2007:2021) %>% 
  mutate(week = lubridate::week(rd), 
         day = lubridate::wday(rd, label = TRUE),
         day = factor(day, levels = rev(levels(day)))) %>% 
  count(PrimaryViolation, year, week, day) %>% 
  gh_waffle() +
  labs(title = "**<i style='color:#28A87D;'>Burglaries</i>** in Sacramento County have _**decreased**_ over time",
       subtitle = "*and tend to occur more on Mondays*")
p3



# write -------------------------------------------------------------------
dir.create(here("plots"))
ggsave(here("plots/p1.png"), p)
ggsave(here("plots/p2.png"), p2)
ggsave(here("plots/p3.png"), p3, width = 15, height = 9)
