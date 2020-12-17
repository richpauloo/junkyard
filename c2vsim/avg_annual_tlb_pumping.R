library(tidyverse)
library(lubridate)

d <- map_df(14:21, ~readxl::read_xlsx("~/Desktop/C2VSim_CG_1972IC_Groundwater budget.xlsx", sheet = .x, skip = 4))

d %>% 
  mutate(month = lubridate::month(as.Date(Time), label = TRUE)) %>% 
  group_by(month) %>% 
  summarise(avg_p = mean(`Pumping (-)`)) %>% 
  ungroup() %>% 
  ggplot(aes(month, avg_p/1000)) +
  geom_col() +
  labs(title = "C2VSim subregions 14-21, Tulare Basin",
       subtitle = "1972-10-31 to 2009-09-30",
       x = "", y = "Average pumping (TAF)")
