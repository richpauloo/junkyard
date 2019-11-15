library(tidyverse)
library(readxl)

d <- list()

for(i in 1:21){
  d[[i]] <- read_excel("F:/Box Sync/Research/Pre QE Research/C2VSim/C2VSim_CG_1921IC_R374_Model/C2VSim_CG_1921IC_R374/Excel/C2VSim_CG_1921IC_Groundwater budget.xlsx", sheet = 1, col_names = TRUE, skip = 4)
  d[[i]]$subregion <- i
}

d <- bind_rows(d)


sjv <- mutate(d, Time = lubridate::ymd(Time)) %>% 
  filter(Time > lubridate::ymd("1988-01-01") & 
           Time < lubridate::ymd("2009-01-01") & 
           subregion %in% 10:21) %>% 
  mutate(year = lubridate::year(Time)) %>% 
  group_by(year) %>% 
  summarise(mean_sub_af = mean(`Net Subsurface Inflow (+)`)) %>%
  ungroup()
  
p <- sjv %>% 
  ggplot(aes(year, mean_sub_af)) + 
  geom_line() + 
  geom_hline(aes(yintercept = mean(sjv$mean_sub_af)), linetype = "dashed", color = "red") +
  geom_text(aes(x = 1992, 
                y = -1650, 
                label = paste("Average net subsurface inflow = ", 
                              round(mean(sjv$mean_sub_af)),
                              " acre feet")
                ),
            check_overlap = TRUE,
            color = "red") +
  labs(x = "Year", 
       y = "Mean subsurface inflow (acre-feet)", 
       title = "Average annual subsurface inflow in the SJV (1988-2008)", 
       subtitle = "Negative values indicate a net export of subsufrace flow",
       caption = "Values calcualted from C2VSim subregions 10-21 (Brush, 2012)") 
ggsave(p, filename="~/p.png")
