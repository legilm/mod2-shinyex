# Shiny app for Airbnb listings in Rio de Janeiro
library(shiny)
library(tidyverse)
library(rio)
library(shinydashboard)
library(bslib)
library(histoslider)
library(plotly)

# Load data
listings <- rio::import("data/listings.csv")

# Clean the price column
listings$price <- as.numeric(sub(",", ".", listings$price, fixed = TRUE)) # Replace comma by dot
listings$price <- as.numeric(gsub("[^0-9.,]", "", listings$price)) # Remove non-numeric characters
listings <- listings %>%
  drop_na(price) %>%
  filter(price > 0 & price < quantile(price, 0.99)) # Remove outliers


# Define UI
ui <- page_sidebar(
  title = "Rio Airbnb Dashboard",
  sidebar = sidebar(
    helpText("This dashboard shows the distribution of prices for Airbnb listings in Rio de Janeiro."),
     sliderInput(
      "bins",
      label = "Number of bins:",
      min = 5,
      max = 50,
      value = 25
     ),
    selectInput(
      "color", 
      label = "Choose a color:", 
      choices = 
        c("Green" = "#00c244", 
          "Blue" = "#007bc2")),
    selectInput(
      "theme", 
      label = "Choose a theme:", 
      choices = 
        c("Classic", 
          "Minimal", 
          "Dark")),
  ),

  
  fluidRow(
    column(4,
           card(
             card_header("Mean Price"),
             textOutput("mean_price"))),
    column(4,
           card(
           card_header("Median Price"),
           textOutput("median_price"))),
    column(4,
            card(
              card_header("Listings Count"),
              textOutput("listings_count"))),
  ),
  
  fluidRow(
    column(4,
           card(
             card_header("Price Distribution"),
             plotOutput("distPlot"))),
    column(4,
           card(
             card_header("Density Plot"),
             plotOutput("densityPlot"))),
    column(4,
           card(
             card_header("Box Plot"),
             plotOutput("boxPlot"))),
  ),
)


# Define server logic -----------------------------------------------------
server <- function(input, output) {
 theme_choice <- reactive({
   switch(input$theme,
          "Classic" = theme_classic(),
          "Minimal" = theme_minimal(),
          "Dark" = theme_dark())
 })
 
 output$mean_price <- renderText({
   paste("R$", round(mean(listings$price), 2))
   })
   
   output$median_price <- renderText({
     paste("R$", round(median(listings$price), 2))
   })
   
   output$listings_count <- renderText({
     paste(nrow(listings))
   })
   
   
   output$distPlot <- renderPlot({
     x <- listings$price
     bins <- seq(min(x), max(x), length.out = input$bins + 1)
     
     ggplot(data.frame(x), aes(x)) +
       geom_histogram(breaks = bins, fill = input$color, color = "white") +
       labs(x = "Price of the listings, in Brazilian Reais (R$)", y = "Frequency") +
       theme_choice()
   })
   
   output$densityPlot <- renderPlot({
     x <- listings$price
     
     ggplot(data.frame(x), aes(x)) +
       geom_density(fill = input$color, alpha = 0.5) +
       labs(title = "Density Plot of Prices, in Brazilian Reais (R$)", x = "Prices (R$)", y = "Density") +
       theme_choice()
   })
   
   output$boxPlot <- renderPlot({
     x <- listings$price
     
     ggplot(data.frame(x), aes(y = x)) +
       geom_boxplot(fill = input$color, color = "black") +
       labs(title = "Boxplot of Prices, in Brazilian Reais (R$)", y = "Prices (R$)") +
       theme_choice()
   })
 }


# Run the application
shinyApp(ui, server, options = list(display.mode = "showcase"))