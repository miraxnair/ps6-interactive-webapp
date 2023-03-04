#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(rsconnect)
library(colourpicker)
library(shiny)
library(dplyr)
library(tidyverse)

gtemp <- read_delim("UAH-lower-troposphere-long.csv.bz2")
newg <- unique(gtemp$region)
num <- unique(gtemp$year)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Global Temperature Data"),

   mainPanel(
      tabsetPanel(
        tabPanel("About", p("This app will create",
          strong("plots and tables"),
          "for the data collected by",
          em("UAH")),
          p("This dataset has 14310 row and 4 columns."),
          h2("Here is a small sample of the dataset:"),
          verbatimTextOutput("Sample")
          ),
    
        tabPanel("Plot", 
                 sidebarPanel("The graph below shows the temperature of different
                 regions over a period of years. Please choose a region below.",
                               radioButtons("typeInput", "Region",
                              choices = c(newg)),
                              radioButtons("color", "Choose color", 
                                           choices = c("skyblue", "lawngreen", "orangered",
                                                                "purple", "gold"))
                              ),
                mainPanel(plotOutput("Plots"),
                          "There are", 
                          verbatimTextOutput("text"),
                          "variables")
                 ),
        tabPanel("Table", 
                 sidebarPanel("This panel finds the average temperature for 
                 each year and you can click on the year that you want to see.",
                              radioButtons("Year", "Select Year",
                                          choices = c(num))
                              ),
                 mainPanel(tableOutput("table"),
                           verbatimTextOutput("result"))
      )
    )
  )
)  




# Define server logic required to draw a histogram
server <- function(input, output) {
  output$Sample <- renderPrint({
    head(gtemp)
  })
  output$Plots <- renderPlot({
    filtered <-
      gtemp%>%
      filter(region == input$typeInput)
    filtered%>%
    ggplot(aes(year, temp)) +
      geom_point(col = input$color)
  })
  output$text <- renderPrint({
   gtemp%>%
      filter(region == input$typeInput)%>%
      nrow()
    
  })
  
  
  output$table <- renderTable({
    yearFilter <- subset(gtemp, gtemp$year == input$Year)
  })
  
  temp_range <- reactive({
    year_data <- subset(gtemp, gtemp$year == input$Year)
    range(year_data$temp)
  })
  
  output$result <- renderPrint({
      paste("Temperature range for year is", temp_range()[1], "to", temp_range()[2])
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
