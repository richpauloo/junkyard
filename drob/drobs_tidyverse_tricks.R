# @drob's 10 tricks for the tidyverse
# https://www.youtube.com/watch?v=NDHSBUN_rVU

library(tidyverse)

# 1. count ----------------------------------------------------------------

mtcars %>% 
  mutate(make = stringr::word(rownames(., 1))) %>% 
  count(make, 
        sort = TRUE, 
        wt = mpg, # if a variable, computes sum for each group, else count
        name = "total_mpg") # rename the "n" column


# 2. create a new variable in group_by() or count() -----------------------

mtcars %>% 
  count(make = stringr::word(rownames(., 1)), sort = TRUE)


# 3a. add_count() ---------------------------------------------------------

# mutate a count per observation:
mtcars %>% 
  group_by(cyl) %>% 
  mutate(n = n()) %>% 
  ungroup()

# shorter form:
mtcars %>% 
  add_count(cyl)


# 3b. add_count() + filter() ----------------------------------------------

# useful to take the largest groups by count then plot
mtcars %>% 
  mutate(make = stringr::word(rownames(., 1))) %>% 
  add_count(cyl) %>% 
  filter(n > 10) %>%  # groups with more than 10
  ggplot(aes(wt, mpg, color = make)) +
  geom_point()



# 4. summarise to create a list-column ------------------------------------
# good to visualize multiple models

restaurants <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018/2018-12-11/nyc_restaurants.csv")

# regular summarise
restaurants %>% 
  group_by(cuisine_description) %>% 
  summarise(score = mean(score, na.rm = TRUE))

# summarise a list column with list(): each output is a t-test
restaurants %>% 
  group_by(cuisine_description) %>% 
  summarise(t_test = list(t.test(score))) %>% 
  mutate(tidied = map(t_test, broom::tidy)) %>% 
  unnest(tidied) %>% 
  filter(estimate > 20) %>% # high violations data
  ggplot(aes(estimate, fct_reorder(cuisine_description, estimate))) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) +
  labs(y = "", x = "Inspection score (higher means more violations)")
  

# 5. favorite plot: count() + fct_reorder() + geom_col() + coord_f --------

# I use this oftern and an example is above in trick 4.


# 6. fct_lump() and fct_reorder() -----------------------------------------

# without fct_lump() and fct_reorder()
restaurants %>% 
  ggplot(aes(score, cuisine_description)) +
  geom_boxplot() 
  labs(y = "", x = "Inspection score (higher means more violations)") 
  
# with fct_lump() and fct_reorder()
restaurants %>% 
  mutate(cuisine_description = fct_lump(cuisine_description, 5),
         cuisine_description = fct_reorder(cuisine_description, score)) %>%
  ggplot(aes(score, cuisine_description)) +
  geom_boxplot() 



# 7. log scales with scale_x/y_log_10 -------------------------------------

# better for visualization and prediction


# 8. crossing() from tidyr ------------------------------------------------

# basically expand.grid(), but returns tibble, doesn't convert strings
# to factors, and can be used to cross two tibbles together

crossing(a = 1:3, 
         b = c("a", "b"),
         c = c("w", "x", "y", "z"))


# 9. separate() -----------------------------------------------------------

# common


# 10. tidyr::extract() ----------------------------------------------------

# kind of like separate, but extracts strings within strings in a column
