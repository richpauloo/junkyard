library(tidyverse)
library(lubridate)

f <- list.files("~/Github/junkyard/COVID-19/csse_covid_19_data/csse_covid_19_daily_reports/", full.names = TRUE)
f <- f[-length(f)]

f2 <- list.files("~/Github/junkyard/COVID-19/csse_covid_19_data/csse_covid_19_daily_reports/")
f2 <- f2[-length(f2)]
f2 <- substr(f2, 1, 10)

l <- vector("list", length = length(f))
for(i in 1:length(f)) {
  l[[i]] <- read_csv(f[i]) 
}

for(i in 1:60) {
  l[[i]] <- l[[i]] %>% 
    rename(Province_State = `Province/State`, 
           Country_Region = `Country/Region`) %>% 
    filter(Province_State %in% c("Hubei", "Seattle, WA") | Country_Region %in% c("Korea, South", "South Korea")) %>% 
    dplyr::select(Province_State, Country_Region, Confirmed, Deaths) %>% 
    mutate(date = f2[i])
}

for(i in 61:length(f)){
  l[[i]] <- l[[i]] %>% 
    filter(Province_State == "Washington" | Country_Region %in% c("Korea, South", "South Korea")) %>% 
    dplyr::select(Province_State, Country_Region, Confirmed, Deaths) %>% 
    mutate(date = f2[i])
  l[[i]] <- l[[i]] %>% 
    group_by(date, Province_State, Country_Region) %>% 
    summarise(Confirmed = sum(Confirmed),
              Deaths = sum(Deaths))
}

l <- bind_rows(l) 
l <- mutate(l, Province_State = ifelse(Country_Region %in% c("Korea, South", "South Korea"), "South Korea", Province_State))

# l <- l %>% 
#   mutate(date = mdy(date),
#          Province_State = str_replace_all(Province_State, "Seattle, WA", "Washington")) %>% 
#   tidyr::gather(metric, value, c("Confirmed", "Deaths")) 

# p <- l %>% 
#   ggplot(aes(date, value, color = metric)) + 
#   geom_point() +
#   facet_wrap(~Province_State, scales="free") +
#   scale_y_log10()
# p 

china <- filter(l, Province_State == "Hubei") %>% 
  rename(state = Province_State, cases = Confirmed, deaths = Deaths) %>% 
  select(-Country_Region) %>% 
  mutate(date = lubridate::mdy(date)) %>% 
  filter(date >= mdy("01-01-2020")) %>% 
  mutate(state = "China")

# s korea
sk <- filter(l, Province_State == "South Korea") %>% 
  rename(state = Province_State, cases = Confirmed, deaths = Deaths) %>% 
  select(-Country_Region) %>% 
  mutate(date = lubridate::mdy(date)) %>% 
  filter(date >= mdy("02-15-2020")) %>% 
  mutate(state = "South Korea")
  
#ggplot(china, aes(date, cases)) + geom_point() + scale_y_log10()


# washington
w <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv") %>% 
  filter(state == "Washington") %>% 
  filter(date >= lubridate::mdy("02-29-2020")) %>% 
  select(-fips) %>% 
  filter(date >= mdy("03-01-2020")) %>% 
  mutate(state = "Seattle, Washington")

#ggplot(w, aes(date, cases)) + geom_point() + scale_y_log10()



bind_rows(china, w, sk) %>% ggplot(aes(date, cases)) + geom_line() + facet_wrap(~state, scales ="free") + scale_y_log10() + theme_minimal() + labs(title = "Confirmed COVID cases", subtitle = "log scale", x="Date", y = NULL)

p1 <- china %>% 
  tidyr::gather(metric, value, c("cases","deaths")) %>% 
  ggplot(aes(date, value, color = metric)) + 
  geom_line() + 
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) + 
  theme_minimal(base_size = 15) + 
  labs(title = "China COVID cases and deaths (log scale)",
       x="Date", y = NULL, color = NULL) 
p1

p2 <- sk %>% 
  tidyr::gather(metric, value, c("cases","deaths")) %>% 
  ggplot(aes(date, value, color = metric)) + 
  geom_line() + 
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) + 
  theme_minimal(base_size = 15) + 
  labs(title = "South Korea COVID cases and deaths (log scale)",
       x="Date", y = NULL, color = NULL) 
p2

p3 <- w %>% 
  tidyr::gather(metric, value, c("cases","deaths")) %>% 
  ggplot(aes(date, value, color = metric)) + 
  geom_line() + 
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) + 
  theme_minimal(base_size = 15) + 
  labs(title = "Seattle, Washington COVID cases and deaths (log scale)",
       x="Date", y = NULL, color = NULL) 
p3

ggsave(p1, filename = "~/Desktop/covid/china.png", dpi = 300)
ggsave(p2, filename = "~/Desktop/covid/sk.png", dpi = 300, height = 4, width = 7.71)
ggsave(p3, filename = "~/Desktop/covid/seattle.png", dpi = 300)

p4 <- bind_rows(china, w) %>% 
  tidyr::gather(metric, value, c("cases","deaths")) %>% 
  ggplot(aes(date, value, color = metric)) + 
  geom_line() + 
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) + 
  theme_minimal(base_size = 15) + 
  labs(title = "COVID cases and deaths (log scale)",
       x="Date", y = NULL, color = NULL) +
  facet_wrap(~state, nrow = 2, scales = "free") +
  theme(panel.grid.minor = element_blank())

ggsave(p4, filename = "~/Desktop/covid/china_seattle.png", dpi = 300)
