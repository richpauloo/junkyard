library(arrow)
library(duckdb)
library(fs)
library(tidyverse)
library(DBI)
library(glue)

# set a working directory on the desktop - make sure nothing else
# is in this directory except the parquet files we are about to write!
dir_out <- "~/Desktop/duck_parquet/my_data"

# create the directory
dir_create(dir_out)

# write dataframe to parquet with partitioning by unique values in a column.
# need values for all rows otherwise NA values go to their own file 
iris_clean <- janitor::clean_names(iris) # make clean names first
arrow::write_dataset(iris, dir_out, partitioning = "species")
dir_ls(dir_out)

# each parquet file is stored in its own subdirecctory (by partition) in a 
# series of parquet files. becauase there are only 50 rows per species, there
# is only one parquet file per subdirectory
dir_ls(dir_out, recurse = TRUE, glob = "*.parquet") 

# we can query the parquet files by pointing arrow::open_dataset to the 
# directory that holds the parquet partions, and use dbplyr syntax that ends
# with a call to collect(). Note that the Species column, which we partioned 
# by is overwritten with a special character string that corresponds to the
# subdirectory names (e.g., `dir_ls(dir_out)`)
ds <- arrow::open_dataset(dir_out, partitioning = "species") 

ds %>% 
  filter(species == "species=setosa") %>%
  count(sepal_length) %>% 
  collect()
  
# at other times, we may want to step away from dbplyr syntax and use pure
# SQL to query our parquet files. duckdb is the tool of choice for this, and 
# allows us to use SQL to query parquet files. 

ds  <- arrow::open_dataset(dir_out, partitioning = "species")
con <- dbConnect(duckdb::duckdb())
duckdb::duckdb_register_arrow(con, "my_table", ds)
dbGetQuery(con, 
           "SELECT sepal_length, COUNT(*) AS n 
           FROM my_table 
           WHERE species = 'species=setosa' 
           GROUP BY sepal_length")

# clean up
duckdb_unregister(con, "my_table")
dbDisconnect(con)


# refs

# [DuckDB R](https://duckdb.org/docs/api/r)
# [DuckDB Parquet](https://duckdb.org/docs/data/parquet)
# [A helpful Jira ticket](https://issues.apache.org/jira/browse/ARROW-12688?page=com.atlassian.jira.plugin.system.issuetabpanels%3Acomment-tabpanel&focusedCommentId=17381539#comment-17381539)
