library(zip)
library(fs)
library(glue)

url <- "https://data.cnra.ca.gov/dataset/dd9b15f5-6d08-4d8c-bace-37dc761a9c08/resource/c51e0af9-5980-4aa3-8965-e9ea494ad468/download/periodic_gwl_bulkdatadownload.zip"

dir_delete("data"); dir_create("data")

download.file(url, "data/pgwl.zip")
unzip("data/pgwl.zip", exdir = glue("data/{Sys.Date()}"))

file_delete("data/pgwl.zip")
