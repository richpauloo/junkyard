#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(plotly)

# read NTY data from GH
d <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv") %>% 
    group_by(state, date) %>%
    summarise(cases = sum(cases),
              deaths = sum(deaths)) %>%
    ungroup()


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("COVID case and death tracker"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("state", "Choose a state:",
                        unique(d$state),
                        selected = c("New York", "Washington", "California"), 
                        multiple = TRUE
            )
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotlyOutput("case_plot"),
           plotlyOutput("death_plot")
        )
    )
)

# Define server logic 
server <- function(input, output) {

    selected_states <- reactive({ filter(d, state %in% input$state) })
    
    output$case_plot <- renderPlotly({
        
        # create the plot
        p <- ggplot(selected_states(), aes(date, cases, color = state)) +
            geom_line() +
            guides(color = FALSE) +
            scale_y_log10(
                breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
            labs(x = "Date", y = "Cases", title = "Cases")
        
        ggplotly(p)
    })
    
    output$death_plot <- renderPlotly({
        
        # create the plot
        p <- ggplot(selected_states(), aes(date, deaths, color = state)) +
            geom_line() +
            guides(color = FALSE) +
            scale_y_log10(
                breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
            labs(x = "Date", y = "Deaths", title = "Deaths")
        
        ggplotly(p)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
