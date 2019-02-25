# https://data.sfgov.org/Energy-and-Environment/Elevation-Contours/rnbg-2qxw
# https://data.sfgov.org/Public-Safety/Map-of-Police-Department-Incident-Reports-2018-to-/jq29-s5wp

library(tidyverse)
d <- read_csv("C:/Users/rpauloo/Desktop/sf_police_2019_02_24/Police_Department_Incident_Reports__2018_to_Present.csv")

# reorder day of wekk factor
d$`Incident Day of Week` <-  factor(d$`Incident Day of Week`, 
                                    levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# clean duplicate`Incident Category` types
d <- d %>% 
  mutate(`Incident Category` = case_when(`Incident Category` %in% c("Weapons Offence", "Weapons Carrying Etc") ~ "Weapons Offense",
                                         `Incident Category` %in% c("Other Offenses", "Other Miscellaneous", NA) ~ "Other",
                                         `Incident Category` == "Human Trafficking (A), Commercial Sex Acts" ~ "Human Trafficking, Commercial Sex Acts",
                                         `Incident Category` == "Motor Vehicle Theft?" ~ "Motor Vehicle Theft",
                                         `Incident Category` == "Suspicious Occ" ~ "Suspicious",
                                         `Incident Category` == "Family Offense" ~ "Offences Against The Family And Children",
                                         `Incident Category` == "Drug Violation" ~ "Drug Offense",
                                         TRUE ~ `Incident Category`),
         `Incident Subcategory` == ifelse("Theft From Vehicle", "Larceny - From Vehicle", `Incident Subcategory`))




# When during the week do more crimes happen? 
# Crimes peak on Friday.
d %>% 
  ggplot(aes(`Incident Day of Week`, fill = `Police District`)) +
  geom_bar() +
  scale_fill_viridis_d()

# Is there a statistically significant difference?


# most common crimes? 
# larceny accounts for 31% of all crime in SF over the period of record
count(d, `Incident Category`) %>% 
  ggplot(aes(fct_reorder(`Incident Category`, n), n)) +
  geom_col() + 
  coord_flip()

# types of larceny?
# theft from vehicles are the most common
d %>% 
  filter(`Incident Category` == "Larceny Theft") %>% 
  count(`Incident Subcategory`) %>% 
  ggplot(aes(fct_reorder(`Incident Subcategory`, n), n, fill = n)) +
  geom_col() + 
  coord_flip() + 
  scale_fill_viridis_c()

# where are you most likely to get your car bokroken into?
  

# timeseries of crimes
library(lubridate)
d$`Incident Datetime` <- ymd_hms(d$`Incident Datetime`)
d %>% 
  ggplot(aes(`Incident Datetime`, ))

# d3 calendar of crime count
# https://rstudio.github.io/r2d3/articles/gallery/calendar/


# monthly basis?

# crime around holidays

# spatial distribution of crime

# geom_point with size = n

# leaflet of all crimes -> try new plugin for fast rendering of many points
