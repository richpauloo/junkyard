library(tidyverse)

d <- read_csv("~/Downloads/1_SurveyResults_10-14-19 - 1_SurveyResults_10-14-19.csv")

d %>% 
  filter(`Are you interested in taking data science courses offered by the Water Boards Training Academy?` %in% 
           c("Very interested", "Somewhat interested")) %>% 
  pivot_longer(CEDEN:`Water49 GIS Library`,
               names_to = "database",
               values_to = "response") %>% 
  filter(!is.na(response)) %>% 
  count(database, response) %>% 
  filter(response != "I do not use this data source") %>% 
  ggplot(aes(fct_reorder(response, n), n, fill = factor(response))) + 
  geom_col(position = "dodge") +
  scale_fill_viridis_d() +
  facet_wrap(~database) +
  coord_flip() +
  labs(y = "Count", x = "", 
       title = "Database use",
       subtitle = "among respondents who were somewhat to very interested in training") +
  theme_gray() +
  guides(fill = FALSE)


d %>% 
  filter(`Are you interested in taking data science courses offered by the Water Boards Training Academy?` %in% 
           c("Very interested", "Somewhat interested")) %>% 
  pivot_longer(`Collect and combine data from different sources - How often do you perform this task?`:`Use machine learning to automate tasks and make predictions - Are you interested in further developing this skill set?`,
               names_to = "question",
               values_to = "response") %>% 
  filter(!is.na(response)) %>%
  separate("question", into = c("task", "question"), " - ") %>% 
  count(question, task, response) %>% 
  mutate(question = str_wrap(question, 20),
         task = str_wrap(task, 25),
         response = ifelse(response == "No experience", "No Experience", response),
         response = factor(response, levels = c(
           "Not interested", "Slightly interested", "Somewhat interested", "Very interested", 
           "Never", "Daily", "Weekly", "Monthly", "Annually",               
           "No Experience", "Basic", "Proficient","Expert" ))
         ) %>% 
  # filter(response != "I do not use this data source") %>% 
  ggplot(aes(response, n, fill = response)) + 
  geom_col(position = "dodge") +
  scale_fill_viridis_d() +
  facet_grid(question~task, scales = "free_y") +
  coord_flip() +
  labs(y = "Count", x = "", 
       title = "Subject matter interest",
       subtitle = "among respondents who were somewhat to very interested in training") +
  theme_gray() +
  guides(fill = FALSE)



d %>% 
  filter(`Are you interested in taking data science courses offered by the Water Boards Training Academy?` %in% 
           c("Very interested", "Somewhat interested")) %>% 
  pivot_longer(`Microsoft Excel - What is your skill level with this software or computer language?`:`ArcGIS - Are you interested in further developing your skills with this software or computer language?`,
               names_to = "question",
               values_to = "response") %>% 
  filter(!is.na(response)) %>%
  separate("question", into = c("skill", "question"), " - ") %>% 
  count(question, skill, response) %>% 
  mutate(question = str_wrap(question, 40),
         response = factor(response, levels = c(
           "Not interested", "Slightly interested", "Somewhat interested", "Very interested", 
           "No Experience", "Basic", "Proficient","Expert" ))
  ) %>% 
  # filter(response != "I do not use this data source") %>% 
  ggplot(aes(response, n, fill = response)) + 
  geom_col(position = "dodge") +
  scale_fill_viridis_d() +
  facet_grid(question~skill, scales = "free_y") +
  coord_flip() +
  labs(y = "Count", x = "", 
       title = "Software interest and skill",
       subtitle = "among respondents who were somewhat to very interested in training") +
  theme_gray() +
  guides(fill = FALSE)

