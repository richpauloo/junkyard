library(duckdb)
library(pins)
library(arrow, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(tictoc) # for timing

dir <- "~/Desktop/nyc-taxi"

arrow::arrow_with_s3()
dir.create(dir)
arrow::copy_files("s3://ursa-labs-taxi-data",dir)
# warning that downloads 37Gb of data!

tic()
ds <- open_dataset(dir, partitioning = c("year", "month"))
toc() # just to show that this is a fast process

fs::dir_info(dir, recurse = TRUE) %>%
  filter(type == "file") %>% 
  summarise(n = n(), size = sum(size)) 

tic()
ds %>% 
  filter(total_amount > 100) %>%
  select(tip_amount, total_amount, passenger_count) %>%
  # calculate a new column, on disk!
  mutate(tip_pct = 100 * tip_amount / total_amount) %>%
  # arrow::to_duckdb() %>% 
  group_by(passenger_count) %>%
  summarise(
    mean_tip_pct = mean(tip_pct),
    n = n()
  ) %>%
  collect()
toc()