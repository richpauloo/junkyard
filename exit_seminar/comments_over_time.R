library(tidyverse)
library(lubridate)
library(plotly)

# read zoom comments from txt file
d <- read_delim("~/Documents/Zoom/2020-06-09 14.41.30 Rich Pauloo's PhD exit seminar 93793074818/chat.txt", delim = "\t", col_names = c("t","m"))

# clean up messages and convert time to minutes
d$m <- str_remove_all(d$m, " From ") %>% str_wrap(40)
d$t <- paste0("2020-06-09 ", d$t) %>% as_datetime()
d$t <- as.numeric((d$t - d$t[1])/60)
d$y <- 0
d$r <- as.factor(rnorm(nrow(d)))

# plot
p <- ggplot(d, aes(x=t, y=y, text = m, color = r)) +
  geom_jitter(width = 0, alpha = 0.5, size = 5) +
  labs(x = "time (minutes)", y = "", 
       title = "Zoom messages during Rich's PhD exit seminar",
       subtitle = "(hover over points to read messages)") +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank(), 
        panel.grid.major.y = element_blank(), 
        legend.position = "none") 

p2 <- ggplotly(p, tooltip = "text") %>% 
  config(displayModeBar = FALSE) %>% 
  layout(title = list(text = paste0("Zoom messages during Rich's PhD exit seminar",
                                    '<br>',
                                    '<sup>',
                                    '(hover over points to read messages)',
                                    '</sup>')))

# export to JSON
plotly::plotly_json(plotly_build(p2), FALSE) %>% 
  write_lines("plotly.json")
getwd()
