library(tidyverse)

# metadata from kaggle
d <- read_csv("data/all_sources_metadata_2020-03-13.csv")
d <- filter(d, !is.na(abstract))

# key words
kw <- c("incubation period","asymptomatic shedding|asymptomatic transmission",
        "seasonality","charge distribution","hydrophilic|hydrophobic",
        "viral shedding","persistence|persist|persists", "natural history",
        "diagnostic|diagnostics","disease model","animal model","adapt",
        "immune|immunity","social distancing","personal protective equipment",
        "environment|environmental"
        )

# find matches
l <- vector("list", length = length(kw))
for(i in 1:length(l)){
  l[[i]] <- d[str_which(tolower(d$abstract), kw[i]), ]$abstract
}

# plot counts
tibble(kw = kw, count = sapply(l, length)) %>% 
  ggplot(aes(fct_reorder(kw, count), count)) + 
  geom_col() +
  coord_flip() +
  labs(y = "Count", x = "")
