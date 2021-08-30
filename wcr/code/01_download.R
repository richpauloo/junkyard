library(tidyverse)
library(here)

# dir structure
dir.create(here("data"))
dir.create(here("data_output"))

# links
url_wcr <- "https://data.cnra.ca.gov/dataset/647afc02-8954-426d-aabd-eff418d2652c/resource/8da7b93b-4e69-495d-9caa-335691a1896b/download/wellcompletionreports.csv"
url_casing <- "https://data.cnra.ca.gov/dataset/647afc02-8954-426d-aabd-eff418d2652c/resource/93fddfef-8c92-4ea1-b6c8-980997bb5fb8/download/casingdata.csv"
url_links <- "https://data.cnra.ca.gov/dataset/647afc02-8954-426d-aabd-eff418d2652c/resource/bff565b3-b3b4-4727-b10b-e09e0012ec3b/download/wellreportpdflinks.csv"
url_borehole <- "https://data.cnra.ca.gov/dataset/647afc02-8954-426d-aabd-eff418d2652c/resource/735030c5-3d4f-428f-a5c7-c258cacdc307/download/boreholeinformation.csv"
url <- list("wcr" = url_wcr, 
            "casing" = url_casing, 
            "links" = url_links, 
            "borehole" = url_borehole) 

# download all data
options(timeout = max(1000, getOption("timeout")))
walk2(url, names(url), ~download.file(.x, here(glue::glue("data/{.y}.csv"))))
