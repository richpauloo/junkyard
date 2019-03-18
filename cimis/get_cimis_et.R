library(xml2)



key       <- ""
targets   <- "2,125"
startdate <- "2019-03-01"
enddate   <- "2019-03-03"
dataitems <- "day-asce-eto"

g <- paste0("http://et.water.ca.gov/api/data?appKey=",
            key,
            "&targets=",
            targets, 
            "&startDate=",
            startdate,
            "&endDate=",
            enddate,
            "&dataItems=",
            dataitems)
g
r <- GET(g) # response
content(r)  # content

x <- read_xml("F:/Downloads/data (15)") # works for a downloaded XML file

##########################################################################
# ET values
##########################################################################
v <- x %>% 
  xml_children() %>% 
  xml_children() %>% 
  xml_children() %>% 
  xml_contents() %>% 
  xml_text()

##########################################################################
# get all attributes 
##########################################################################

# attributes to scrape
ats <- x %>% 
  xml_children() %>% 
  xml_children() %>% 
  xml_children() %>% 
  xml_attrs() %>% 
  .[[1]] %>% 
  names()

# scrape attributes and compile in df
d <- vector("list", length = length(ats))
for(i in 1:length(d)){
  d[[i]] <- x %>% 
    xml_children() %>% 
    xml_children() %>% 
    xml_children() %>% 
    xml_attr(ats[i])
}
d <- as.data.frame(d, col.names = ats)

# join with values
d$et <- v
  
