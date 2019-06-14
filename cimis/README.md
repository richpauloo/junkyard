## Scripts to scrape [CIMIS ET data](https://cimis.water.ca.gov/).  

`ftp_historical_data.R` scrapes, organizes, and lightly cleans all daily historical data via [FTP](ftp://ftpcimis.water.ca.gov/pub2/). Note that before and after June 2014, column designation changes. See `readmes` to compare daily measurements.  

`get_cimis_et.R` scapes the last 30 days of reference ET in California's Central Valley, interpolates over a grid, and creates an animation of the spatial data sube, and spatially-averaged timeseries.  

`clean_cimis_historical.R` is an unfinished script to clean the historical scraped data.  
