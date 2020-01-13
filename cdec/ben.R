# Scrape CDEC reservoir outflow data from
# THIS website: https://cdec.water.ca.gov/dynamicapp/wsSensorData

# CDEC gives reservior outflow data at a temporal resolution as fine as the hour, 
# so let's grab that for the past year

# packages
library(tidyverse) # general purpose DS toolkit
library(rvest)     # for webscraping
library(viridis)   # colorblind-safe color palettes
library(readr)     # fast read/write functions

# scrape CDEC station codes
station_codes <- read_html('http://cdec.water.ca.gov/misc/resinfo.html') %>% 
  html_table() %>% 
  .[[1]] %>% 
  pull(ID)

length(station_codes) # 199 unique reservoirs

# starting and ending time indices
start <- '2018-01-01'               # start of query
end   <- '2019-01-24'               # today

# constrct the url to query CDEC
url <- paste0('https://cdec.water.ca.gov/dynamicapp/req/CSVDataServlet?Stations=', 
              paste(station_codes, collapse = '%2C'),
              '&SensorNums=23&dur_code=H&Start=',       # 23 indicates hourly outflow
              start,
              '&End=',
              end)

# read the generated webpage, clean the date column, and omit missing values for the present day
df <- read.csv(url)

df <- df %>% 
  mutate(year = substr(df$DATE.TIME, 1, 4), 
         month = substr(df$DATE.TIME, 5, 6), 
         day = substr(df$DATE.TIME, 7, 8), 
         hour = substr(df$DATE.TIME, 10, 13),
         ymdhm = lubridate::ymd_hm(paste(year, month, day, hour, sep = "-"))) %>% 
  filter(VALUE != "---") %>% 
  mutate(VALUE = as.numeric(VALUE),
         year = as.numeric(year), 
         month = as.numeric(month), 
         day = as.numeric(day))

# how many reservoirs have hourly outflow data?
unique(df$STATION_ID) %>% length() # 27

 
# Visualize  
  
# There are 199 station codes, and not all of them have reservoir outflow data
# so let's just look at the hourly outflow of 2 reservoirs: Folsom, Shasta
# let's also just look at the 24 hour interval on 2018-01-01
# Reservoir outflow in Shasta peaking around 8pm. Hydropeaking? 

df %>%
  filter(STATION_ID %in% c("FOL", "SHA") &
         year == 2018 &
         month == 1 &
         day == 1) %>% 
  ggplot(aes(ymdhm, VALUE, color = STATION_ID)) + 
  geom_line(size = 1) + 
  scale_color_viridis_d() +
  labs(title = "Hourly Outflow",
       subtitle = "Folsom and Shasta Reservoir",
       x = "Date", y = "Outflow (CFS)") +
  theme_minimal()
