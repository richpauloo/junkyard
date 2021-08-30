library(tidyverse)
library(here)

# read data and join it all together
l <- list.files(here("data"), full.names = TRUE) %>% 
  map(~read_csv(.x) %>% janitor::clean_names())
names(l[[1]]) <- str_replace_all(names(l[[1]]), "wcr", "wcr_")
names(l[[2]]) <- str_replace_all(names(l[[2]]), "wcr", "wcr_")
l <- reduce(l, full_join, by = "wcr_number") 

# fix some duplicate columns
l <- l %>%
  rename(intervalstart = intervalstart.x,
         intervalend = intervalend.x) %>%
  select(-c(intervalend.y, intervalstart.y))

# de-duplicate
l <- unique(l)


