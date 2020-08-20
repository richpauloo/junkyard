library(tidyverse)
library(plotly)

# read NTY data from GH
d <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")

p <- d %>%
  #filter(state %in% c("California", "Washington", "New York")) %>%
  group_by(state, date) %>%
  summarise(cases = sum(cases),
            deaths = sum(deaths)) %>%
  ungroup() %>%
  ggplot(aes(date, cases, color = state)) +
  geom_line() +
  guides(color = FALSE) +
  scale_y_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  labs(x = "Date", y = "Cases")

p

ggplotly(p)