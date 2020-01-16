# Scrape CDEC reservoir outflow data from
# THIS website: https://cdec.water.ca.gov/dynamicapp/wsSensorData

# CDEC gives reservior outflow data at a temporal resolution as fine as the hour, 
# so let's grab that for the past year

# packages
library(tidyverse) # general purpose DS toolkit
library(rvest)     # for webscraping
library(viridis)   # colorblind-safe color palettes
library(readr)     # fast read/write functions

# all station capacity
# seems to be something for major 12 reservoirs, but nothing comprehensive
# https://www.americangeosciences.org/critical-issues/maps/interactive-map-water-levels-major-reservoirs-california
# https://cdec.water.ca.gov/resapp/RescondMain
# https://engaging-data.com/ca-reservoir-dashboard/
# missing from: https://www.farmwater.org/farm-water-news/californiareservoirs/
res_capacity <- 


# scrape CDEC station codes
station_codes <- read_html('http://cdec.water.ca.gov/misc/resinfo.html') %>% 
  html_table() %>% 
  .[[1]] %>% 
  pull(ID)

length(station_codes) # 199 unique reservoirs

# 30 day data window
start <- Sys.Date() - 365            # start of query
end   <- Sys.Date()                 # today

# constrct the url to query CDEC
url <- paste0('https://cdec.water.ca.gov/dynamicapp/req/CSVDataServlet?Stations=', 
              paste(station_codes, collapse = '%2C'),
              '&SensorNums=15&dur_code=D&Start=',       # 15 = storage, D = daily
              start,
              '&End=',
              end)

# read the generated webpage, clean the date column, and omit missing values for the present day
df <- read_csv(url) %>% 
  rename(value = VALUE, id = STATION_ID, dt = `DATE TIME`)
  
df <- df %>% 
  mutate(year = substr(df$dt, 1, 4), 
         month = substr(df$dt, 5, 6), 
         day = substr(df$dt, 7, 8),
         value = as.numeric(value)
  ) %>% 
  select(id, dt, value, year, month, day)
  

# reservoirs with daily storage data
unique(df$id) %>% length() # 81

 
# Visualize  
  
# There are 199 station codes, and not all of them have reservoir outflow data
# so let's just look at the hourly outflow of 2 reservoirs: Folsom, Shasta
# let's also just look at the 24 hour interval on 2018-01-01
# Reservoir outflow in Shasta peaking around 8pm. Hydropeaking? 

df %>%
  filter(id %in% c("SHA")) %>% 
  ggplot(aes(dt, value/1e6, color = id)) + 
  geom_line(size = 1) + 
  geom_hline(yintercept = 4552000/1e6) +
  scale_color_viridis_d() +
  labs(title = "Daily Storage",
       subtitle = "Folsom and Shasta Reservoir",
       x = "Date", y = "Storage (MAF)") +
  theme_minimal() +
  facet_wrap(~id, ncol=1, scales = "free_y") 
