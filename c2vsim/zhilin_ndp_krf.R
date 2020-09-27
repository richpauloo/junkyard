library(tidyverse)

# krf
krf_area_km2 = 15 * 12.6

# subbasin 17
sb17_acres <- 372889.22
sb17_km2   <- sb17_acres* 0.00404686

# scaling ratio
sr <- krf_area_km2 / sb17_km2

# sb17 data
d <- readxl::read_xlsx("~/Downloads/C2VSim_CG_1972IC_R374_rev/Excel/C2VSim_CG_1972IC_Groundwater budget.xlsx",
                       sheet = 17, skip = 4)

# data range
range(d$Time)

d <- d %>% 
  mutate(month  = lubridate::month(Time),
         year   = lubridate::year(Time),
         season = ifelse(month %in% 4:9, "irrigation", "fallow")) 

group_by(d, season, year) %>% 
  summarise(mean_ndp_taf = mean(`Net Deep Percolation (+)`)/1000) %>% 
  ggplot(aes(year, mean_ndp_taf*sr, color = season)) +
  geom_point() +
  geom_line() +
  labs(x = "Year", y = "Mean NDP (TAF)",
       title = "KRF NDP per season") +
  theme_minimal()

# mean fallow and irrigation season NDP in sb17
group_by(d, season) %>% 
  summarise(mean_ndp_taf = mean(`Net Deep Percolation (+)`)/1000) %>% 
  ungroup() %>% 
  mutate(mean_ndp_taf_KRF = mean_ndp_taf * sr) %>% 
  knitr::kable()
