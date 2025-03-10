# Improved Version of 'Hello Shiny'
library(shiny)
library(tidyverse)

listings <- read_csv("data/listings.csv")

# Clean the price column
listings$price <- as.numeric(sub(",", ".", listings$price, fixed = TRUE)) # Replace comma by dot
listings$price <- as.numeric(gsub("[^0-9.,]", "", listings$price)) # Remove non-numeric characters
listings <- listings %>%
  drop_na(price) %>%
  filter(price > 0 & price < quantile(price, 0.99)) # Remove outliers


# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Airbnb listings on Rio de Janeiro - Brazil"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins", "Number of bins:", min = 5, max = 25, value = 10),
      selectInput("color", "Choose a color:", choices = c("Blue" = "#007bc2", "Red" = "#c20000", "Green" = "#00c244")),
      textInput("title", "Enter plot title:", value = "Histogram of Prices")
    ),
    
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    x <- listings$price
    
    if (length(x) == 0) {
      return(NULL)  # check if there are no values to plot
    }
    
    bins <- seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out = input$bins + 1)
    
    hist(x, breaks = bins, col = input$color, border = "white",
         xlab = "Price range of listings",
         main = input$title)
  })
}


# Run the application
shinyApp(ui, server, options = list(display.mode = "showcase"))