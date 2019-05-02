library(pdftools)
library(tidytext)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(reshape2)
library(wordcloud)

d <- pdf_text("C:/Users/rpauloo/Desktop/mueller/mueller-report-searchable.pdf")
d <- str_replace_all(d, "\r\n", " ")

text_df <- tibble(page = 1:length(d), text = d)
df <- text_df %>%
  unnest_tokens(word, text) %>% 
  group_by(page) %>% 
  mutate(page_ind = row_number(page),
         page_length = length(page)) %>% 
  ungroup() %>% 
  mutate(pos_page = page_ind/page_length,
         doc_ind  = 1:nrow(.),
         pos_doc  = doc_ind/nrow(.)) %>% 
  anti_join(stop_words) %>% 
  filter(!str_detect(word, "[:digit:]"))

df_text <- tibble(page = 1:length(d), 
                  text = str_wrap(d, width = 50))

# trump = positive, so we remove this to not skew the overall sentimant of the report
p <- df %>%
  inner_join(get_sentiments("bing")) %>% 
  filter(word != "trump") %>% 
  count(page, sentiment) %>% 
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative,
         sentiment = sentiment/max(abs(sentiment)),
         dir       = ifelse(sentiment >=0, "up","dn")) %>% 
  # left_join(df_text,
  #           by = "page") %>% 
  # ggplot(aes(page, sentiment, fill = dir, label = text)) +
  ggplot(aes(page, sentiment, fill = dir)) +
  geom_col() +
  labs(title    = "The Muller Report is generally negative",
       subtitle = "Relative sentiment per page",
       x = "Page number", 
       y = "Normalized sentiment") +
  coord_cartesian(ylim = c(1,-1)) +
  theme_minimal() +
  theme(legend.position='none') 

p

# buttons_to_remove <- list("zoom2d", "select2d", "lasso2d", "resetScale2d",
#                           "hoverClosestCartesian", "hoverCompareCartesian",
#                           "zoom3d", "pan3d", "resetCameraDefault3d", "resetCameraLastSave3d", "hoverClosest3d",
#                           "orbitRotation", "tableRotation",
#                           "zoomInGeo", "zoomOutGeo", "resetGeo", "hoverClosestGeo",
#                           "sendDataToCloud",
#                           "hoverClosestGl2d",
#                           "hoverClosestPie",
#                           "toggleHover",
#                           "resetViews",
#                           "toggleSpikelines")
# library(plotly)
# ggplotly(p, tooltip = c("label")) %>% 
#          config(collaborate = FALSE, modeBarButtonsToRemove = buttons_to_remove) 


bing_word_counts <- df %>%
  inner_join(get_sentiments("bing")) %>%
  filter(word != "trump") %>% 
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()


df %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))


df %>%
  inner_join(get_sentiments("bing")) %>%
  filter(word != "trump") %>% 
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("red", "lightblue"),
                   max.words = 100)



