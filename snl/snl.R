library(tidyverse)

ch <- read_csv("~/Downloads/archive/characters.csv")
im <- read_csv("~/Downloads/archive/impressions.csv")
tn <- read_csv("~/Downloads/archive/tenure.csv")

head(ch)
head(im)
head(tn)

x <- ch %>% 
  count(aid) %>% 
  left_join(tn) %>% 
  filter(!is.na(n_episodes)) 
  
x2 <- group_by(ch, aid) %>%
  summarise(name = glue::glue_collapse(name, sep = ", ")) %>% 
  ungroup() %>% 
  mutate(name = str_wrap(name, 60)) %>% 
  left_join(tn) %>% 
  filter(!is.na(n_episodes)) 


p <- x %>% 
  ggplot(aes(eps_present, n, size = n)) +
  geom_abline(slope = 1/10, color = "#F2B701", linetype = "dashed", lwd = 1) + 
  geom_abline(slope = 1/5, color = "#7F3C8D", linetype = "dashed", lwd = 1) + 
  geom_point(alpha = 0.6) + 
  geom_point(
    data = filter(x, n > 26 | aid %in% c("Aidy Bryant", "Kate McKinnon")),
    mapping = aes(eps_present, n, size = n),
    fill = "#28A87D",
    pch = 21
  ) +
  ggrepel::geom_label_repel(
    data = filter(x, n > 26 | aid %in% c("Aidy Bryant", "Kate McKinnon")),
    mapping = aes(eps_present, n, label = aid), 
    max.overlaps = Inf, vjust = 0.7
  ) +
  labs(x = "Number of episodes", y = "Number of characters played",
       title = "On average, SNL actors pick up 1 new character for every 10 episodes they appear in",
       subtitle = "Actors like Wigg and Ferrell picked up about 1.5 new characters per 10 episodes",
       caption = "Data from Kaggle: https://www.kaggle.com/hhllcks/snldb \nCreated by @RichPauloo") +
  coord_cartesian(xlim = c(0, 400), ylim = c(0, 50)) +
  guides(size = "none", fill = "none") +
  theme_minimal(base_size = 12) +
  theme(plot.title.position = "plot",
        panel.grid.minor = element_blank())

# number of new characters per episode
b1 <- lm(n ~ eps_present, data = x) %>% 
  broom::tidy() %>% 
  pull(estimate) %>% 
  .[2]

ggsave("~/Desktop/snl.png", p, height = 8, width = 10, dpi = 300)
