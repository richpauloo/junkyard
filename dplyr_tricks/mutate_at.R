# mutate_at new columns while keeping old ones
# https://stackoverflow.com/questions/45947787/create-new-variables-with-mutate-at-while-keeping-the-original-ones


library(dplyr)

iris %>% 
  mutate_at(vars(contains("Sepal")), 
            .funs = list(mean = ~mean(Petal.Length),
                         max = ~max(Petal.Length))) %>% 
  head()
