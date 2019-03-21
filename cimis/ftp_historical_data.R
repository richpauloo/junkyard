##########################################################################
# Download FTP of all historical data, get ET, and join to station
# spatial information
##########################################################################

library(curl)
library(tidyverse)

# location of FTP. grab all the names of the files
url <- "ftp://ftpcimis.water.ca.gov/pub2/annual/"
h   <- new_handle(dirlistonly = TRUE)
con <- curl(url, "r", h)
tbl <- read.table(con, stringsAsFactors = TRUE, fill = TRUE)
close(con)
head(tbl)

# only download the "daily files
urls <- paste0(url, tbl[grepl(pattern = "daily", tbl$V1), 1])
fls  <- basename(urls)

# download to disk
for(i in 1:length(urls)){
  curl_download(urls[i], paste0("C:/Users/rpauloo/Desktop/cimis_et/", fls[i]))
}

# unzip
for(i in 1:length(fls)){
  unzip(paste0("C:/Users/rpauloo/Desktop/cimis_et/", fls[i]), 
        exdir = "C:/Users/rpauloo/Desktop/cimis_et")
}

# 2016 names are messed up
all <- list.files("C:/Users/rpauloo/Desktop/cimis_et/", pattern = ".csv")
bad <- all[nchar(all) == 12] # short file names missing a year
file.rename(from = paste0("C:/Users/rpauloo/Desktop/cimis_et/", bad), 
            to = paste0("C:/Users/rpauloo/Desktop/cimis_et/", "2016", bad))


# read into a list dataframes, and subset for ET data

# column names from CIMIS FTP README
cn <- c(LETTERS[1:26], c("AA","AB","AC","AD","AE")) 
yr <- stringr::str_extract_all(fls, "[:digit:]", simplify = TRUE) %>% 
  apply(., FUN = function(x) paste(x, collapse = ""), MARGIN = 1)


l  <- vector("list", length = length(yr))

# pattern match year at start of filename, read those files in, 
# and bind all rows into a single df per list element
for(i in 1:length(l)){
  files <- list.files("C:/Users/rpauloo/Desktop/cimis_et/", 
                      pattern = paste0("^", yr[i]), 
                      full.names = TRUE)
  
  temp <- vector("list", length = length(files))
  
  for(j in 1:length(files)){
    temp[[j]] <- read_csv(files[j], col_names = cn)
  }
  
  l[[i]] <- do.call(rbind.data.frame, temp) %>% mutate(year = yr[i])
}

df <- do.call(rbind.data.frame, l)

# join to spatial data
spt <- readr::read_rds("C:/Users/rpauloo/Documents/GitHub/junkyard/cimis/data/station_spatial.rds")
spt$StationNbr <- as.character(spt$StationNbr)

df$A %>% unique()

# trim leading zeroes on station ids 
df$A <- sub("^[0]+", "", df$A) 

# finally, join to spatial data
df <- left_join(df, spt, by = c("A" = "StationNbr"))

# sanity check that all stations have a lat/lng
df %>% filter(is.na(lat) | is.na(lng))

# write for collaborators
readr::write_csv(df, "C:/Users/rpauloo/Desktop/cimis_stations_spatial.csv")

