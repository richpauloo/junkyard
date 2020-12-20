# ------------------------------------------------------------------------
# create a directed chord network map
library(tidyverse)
library(sf)
library(here)

# country spatial data from:
# http://worldmap.harvard.edu/data/geonode:country_centroids_az8
countries <- read_csv(here("country_centroids_az8.csv")) %>% 
  st_as_sf(coords = c("Latitude", "Longitude"), crs = 4326) %>% 
  mutate(lc_name = str_to_lower(name)) %>% 
  select(name, lc_name) %>% 
  mutate(lon = unlist(st_coordinates(.))[, 2],
         lat = unlist(st_coordinates(.))[, 1],
         id  = 1:nrow(.)) %>% 
  st_drop_geometry()

nodes2 <- countries

# abstract data
d <- read_csv(here("12_19_directed_chord_diagram.csv")) %>% 
  select(-X1)

# ------------------------------------------------------------------------
# join countries

# unique country data
duc <- c(d$country_mentioned, d$author_country) %>% unique()

# non-matches
duc[!duc %in% countries$lc_name]

# names to remove 
rm_names <- c("hawaii", "reunion", "yugoslavia")

# crosswalk
cw <- tibble(d_name = duc[!duc %in% countries$lc_name], countries_name = NA) %>% 
  filter(!d_name %in% rm_names)

# fuzzy join fails
#fuzzyjoin::stringdist_left_join(cw, select(countries, lc_name), by = c("d_name" = "name"))

# create crosswalk and manually join
# View(select(countries, lc_name))
n <- c("bolivia", "united kingdom", "united states", 
       "korea", "russia", "libya", 
       "cã´te d'ivoire", "czech rep.", "brunei")
fn <- c("bolivia", "united kingdom", "USA", 
        "south korea", "russia", "libya", 
        "cote d'ivoire", "czech republic", "brunei")
cw$countries_name <- n
cw$final_name <- fn

# overwrite edge list with crosswalk and add weights
efp <- d %>% 
  # remove names that are not countries
  filter(!country_mentioned %in% rm_names & !author_country %in% rm_names) %>% 
  # correct two columns with crosswalk so 
  # d$country_mentioned and d$author_country match countries$lc_name
  left_join(cw, by = c("country_mentioned" = "d_name")) %>% 
  mutate(country_mentioned = ifelse(is.na(countries_name), country_mentioned, countries_name)) %>% 
  select(-c("countries_name", "final_name")) %>% 
  left_join(cw, by = c("author_country" = "d_name")) %>% 
  mutate(author_country = ifelse(is.na(countries_name), author_country, countries_name)) %>% 
  select(-c("countries_name", "final_name")) %>%
  # add ids and edge weights data with no loss
  left_join(nodes2, by = c("author_country" = "lc_name")) %>% 
  rename(from = id, x = lon, y = lat) %>% 
  select(-name) %>% 
  left_join(nodes2, by = c("country_mentioned" = "lc_name")) %>% 
  rename(to = id, xend = lon, yend = lat) %>% 
  rename(weight = count) %>% 
  # remove self-referential countries because geom_curve can't handle this
  filter(! from == to) %>% 
  # normalize weight from 0-1
  mutate(weight = weight/max(weight))
  
# set weight threshold
efp2 <- efp %>% filter(weight >= 0.1)
  
# remove nodes with no count and that don't make weight threshold
nodes_with_edges <- nodes2 %>% 
  filter(lc_name %in% efp2$country_mentioned | lc_name %in% efp2$author_country)

# ------------------------------------------------------------------------
# plot

p <- ggplot(nodes_with_edges) + country_shapes +
  geom_curve(aes(x = x, y = y, xend = xend, yend = yend,     # draw edges as arcs
                 color = weight, size = weight),
             data = efp2, curvature = 0.33, alpha = 0.9,
             arrow = arrow(length = unit(efp2$weight, "lines"), type = "closed")) + 
  # scale_color_distiller("Relative weight", palette = "Reds", direction = 1) +
  scale_color_viridis_c("Relative weight", option = "C", direction = -1) +
  scale_size_continuous(guide = FALSE, range = c(0.1, 2)) +  # scale for edge widths
  geom_point(aes(x = lon, y = lat),                          # draw nodes
             shape = 21, size = 1,#nodes$weight,
             fill = 'white', color = 'black', stroke = 0.5) +
  geom_text(aes(x = lon, y = lat, label = name),             # draw text labels
            hjust = 0, nudge_x = 1, nudge_y = 4,
            size = 2, color = "white", fontface = "bold") +
  mapcoords + maptheme + 
  guides(color = guide_colorbar(barwidth = 30, barheight = 1, direction = "horizontal", title.position = "top", title.hjust = 0.5))

ggsave(here("p.png"), p, width = 10, height = 6)


# write nodes and edges
nodes2 %>% write_csv(here("nodes.csv"))
efp %>% write_csv(here("edges.csv"))
