# https://groups.google.com/g/shiny-discuss/c/fZORQAqkKsQ

# packages
library(shiny)
library(leaflet)

# make df for leaflet
mapDF <- data.frame(
  location = c("Central Park Zoo", "Guggenheim Museum", "Natural History Museum"),
  lat = c(40.7678014, 40.7829208, 40.7813281),
  lng = c(-73.971908, -73.9590684, -73.974125),
  hrefValue = c("zoo", "guggenheim", "history")
)


# ui
ui <- tagList(
  # link js
  tags$head(tags$link(includeScript("func.js"))),
  tags$head(tags$style("a{cursor:pointer;}")),
  # UI
  navbarPage(
    
    title = "Shinyapp",
    tabPanel("Home", 
             value ="home",
             titlePanel("Leaflet Example"),
             h4("Creating hrefs to other shiny tabs"),
             leafletOutput("map")
    ),
    tabPanel("Central Park Zoo", 
             value="zoo",
             titlePanel("Central Park Zoo"),
             p("...some text here..."),
             helpText("Would you like to go back? If so click ", 
                      HTML("<a onclick=","customHref('home')" ,">",
                           "here","</a>"))),
    tabPanel("Guggenheim",
             value="guggenheim",
             titlePanel("Guggenheim Museum"),
             p("...some text here..."),
             helpText("Would you like to go back? If so click ", 
                      HTML("<a onclick=","customHref('home')" ,">",
                           "here","</a>"))),
    tabPanel("Natural History Museum",
             value="history",
             titlePanel("Natural History Museum"),
             p("...some text here..."),
             helpText("Would you like to go back? If so click ", 
                      HTML("<a onclick=","customHref('home')" ,">",
                           "here","</a>")))
  )
)

# server
server <- function(input, output){
  
  # make map
  output$map <- renderLeaflet({
    
    leaflet(data = mapDF) %>%
      setView(
        lat = 40.7752739, 
        lng = -73.9688518,
        zoom = 14) %>%
      addTiles() %>%
      addMarkers(lng = ~lng , lat = ~lat,
                 popup = paste0(
                   "Learn more about the ",
                   "<a onclick=","customHref('",mapDF$hrefValue,"')>",
                   mapDF$location,
                   "</a>"
                 ))
  })
}

# launch
shinyApp(ui, server)