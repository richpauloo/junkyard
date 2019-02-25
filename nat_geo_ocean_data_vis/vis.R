# load leaflet library
library(leaflet)

# Set up your data frame
data <- data.frame(c(43.11940,43.11940),
                   c(-79.24658,-79.244658),
                   c("HQ 1","HQ 2"),
                   c(4736583,3204853),
                   c('<iframe width="300" height="169" src="https://blueocean.net/wp-content/uploads/2018/04/Turtle-with-Net-Francis-Perez-World-Press-Photo-awards2017-300x200.jpg" frameborder="0" allowfullscreen></iframe>',
                     '<iframe width="300" height="169" src="https://www.youtube.com/embed/4wH878t78bw" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'))

# give names to your data columns
names(data) <- c("lat","lng","name","score","video")

# create the leaflet map variable
m <- leaflet() %>% 
  addProviderTiles("Esri.WorldTopoMap") %>% 
  addMarkers(data=data,
             lat=~lat,
             lng=~lng,
             popup= ~paste("<h3 style='color: red'>Hello World</h3>","<b>Name:</b>",name,"<br>","<b>Score:</b>",score, video, sep=" ")
  )

# print map
m