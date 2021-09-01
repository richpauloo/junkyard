library(tidyverse)

# directory to recursively search and n packages to plot
path    <- "~/Documents/Github/lwa_sc"
max_pkg <- 10

# extract packages and prep for ggplot
d <- fs::dir_ls(path, recurse = TRUE, glob = "*.R") %>% 
  map(~read_lines(.x) %>% 
        str_extract_all(pattern = "^library\\([a-zA-Z]*\\)") %>% 
        unlist() %>% 
        str_remove_all("library|\\(|\\)") 
      ) %>% 
  reduce(c) %>% 
  tibble(package = .) %>% 
  count(package, sort = TRUE) %>%
  slice(1:max_pkg) %>% 
  mutate(prop = glue::glue("{round(100 * n / sum(n), 1)}%"),
         prop = str_pad(prop, width = 5, "left"),
         col  = c(rcartocolor::carto_pal(6, "Bold")[-6], 
                  grey.colors(nrow(.))[-c((nrow(.)-4):nrow(.))]),
         label = glue::glue("<b style='color:{col};'>{package}</b>"),
         label = factor(label, levels = rev(label))) 

# plot
d %>% 
  ggplot(aes(n, label)) +
  geom_col(fill = d$col) + 
  geom_text(aes(label = prop), nudge_x = -1.5, color = "white") +
  labs(y = "", x = "Count",
       title = glue::glue("**{nrow(d)} most common packages used in:** _{path}_")) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.y = ggtext::element_markdown(),
        plot.title = ggtext::element_markdown(),
        plot.title.position = "plot")

