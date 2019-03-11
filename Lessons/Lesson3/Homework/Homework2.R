library(shiny)
library(dplyr)
library(ggplot2)

data <- read.csv(".lesson2_KPI.csv")

ui <- fluidPage(
  titlePanel("Premium & Expenses"),
  selectInput("select", label = NULL,
              choices = list("Region", "Unit", "Segment", "Business", "Year")),
  plotOutput(outputId = "DataPlot"))

server <- function(input, output) 
{
  output$DataPlot <- renderPlot({
    ggplot(data, aes_string(x = "Premium", y = "Expenses")) +
      geom_point(aes_string(x = data$Premium, y = data$Expenses, colour = input$select)) +
      geom_smooth(aes_string(x = data$Premium, y = data$Expenses, colour = input$select), method = loess, se = FALSE)})
}

shinyApp(ui, server)