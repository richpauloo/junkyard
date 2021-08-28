library(tidyverse)
library(rvest)
library(here)
library(glue)

# sometimes necessary to set a higher internet timeout for large files 
# and/or slow connections
options(timeout = max(1000, getOption("timeout")))

# directory to store downloaded data
dir.create(here("data"))

# url to download from and base download url
url  <- "https://www.sacsheriff.com/pages/crime_report_log.php"
base <- "https://www.sacsheriff.com/documents/crime_logs"
  
# scrape zip files to download 
zf <- read_html(url) %>% 
  html_nodes(".pdf_style") %>% 
  html_attr("href") %>% 
  str_extract_all("Crime.*$")

# years to include in the download url
yr <- str_extract_all(zf, "[0-9]{4}")

# download all zip files
walk2(zf, yr, ~download.file(glue("{base}/{.y}/{.x}"), 
                             here(glue("data/{.x}"))))

# unzip all files and remove zipped files
zf <- list.files(here("data"), full.names = TRUE) 
walk(zf, ~unzip(.x, exdir = here("data")))
unlink(zf)
