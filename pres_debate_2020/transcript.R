library(tidyverse)
library(rvest)

# read transccription p nodeset
text <- read_html("https://www.rev.com/blog/transcripts/donald-trump-joe-biden-1st-presidential-debate-transcript-2020") %>% 
  html_nodes("#transcription p") %>% 
  html_text()

# names and times
nt <- n %>% str_split("\n") %>% map(1) %>% unlist() %>% str_split(": ", simplify = TRUE)
