##########################################################################
# Download FTP of all historical data, get ET, and join to station
# spatial information
##########################################################################

library(curl)

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

# read into a list dataframes, and subset for ET data


# join to spatial data