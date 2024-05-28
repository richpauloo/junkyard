library(tidyverse)
library(fs)
library(glue)
library(data.table)

date <- Sys.Date()
d <- fread(glue("data/2024-05-27/measurements.csv"))
s <- read_csv("data/2024-05-27/stations.csv")

# count of unique site codes
n <- d$site_code %>% unique() %>% length()
cat(n, "unique site codes.\n")

# distribution of site code count
dn <- count(d, site_code, sort = TRUE)
dn %>% 
  ggplot(aes(log(n))) + 
  geom_density()

# wlm method description
d %>% count(wlm_mthd_desc, sort = TRUE)
d %>% count(source, sort = TRUE)

# telem data
dt <- d %>% filter(source == "DWR_CONTINUOUS")

cat(length(unique(dt$site_code)), "unique telem site codes.\n")

# join telem data, station data, and write 
dt <- dt %>% left_join(s, by = "site_code")
write_csv(dt, glue("out/{Sys.Date()}-dwr-continuous.csv"))
