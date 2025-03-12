# Shiny app for Airbnb listings in Rio de Janeiro
library(shiny)
library(tidyverse)
library(shinydashboard)
library(shinyWidgets)
library(DT)

# Load data
listings <- rio::import("data/listings.csv")

# Clean the price column
listings$price <- as.numeric(sub(",", ".", listings$price, fixed = TRUE)) # Replace comma by dot
listings$price <- as.numeric(gsub("[^0-9.,]", "", listings$price)) # Remove non-numeric characters
listings <- listings %>%
  drop_na(price) %>%
  filter(price > 0 & price < quantile(price, 0.99)) # Remove outliers


# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Rio Airbnb Dashboard"),
  dashboardSidebar(
    sliderInput("bins", 
                "Number of bins:",
                min = 5, 
                max = 50, 
                value = 25),
    pickerInput("color", 
                "Choose a color:",
                choices = c("Green" = "#00c244", "Blue" = "#007bc2", "Red" = "#c20000"),
                selected = "#00c244",
                multiple = FALSE,
                options = list(style = "btn-primary")),
    pickerInput("theme", 
                "Choose a theme:", 
                choices = c("Classic", "Minimal", "Dark"),
                selected = "Dark"),
    pickerInput(
                inputId = "neighbourhood_filter", 
                label = "Select the neigghbourhood:", 
                choices = unique(listings$neighbourhood), 
                options = pickerOptions(
                actionsBox = TRUE, 
                size = 10,
                selectedTextFormat = "count > 3"), 
                multiple = TRUE
    )
  ),
  dashboardBody(
    fluidRow(
      valueBox(
        width = 4,
        value = textOutput("mean_price"),
        subtitle = "Mean Price",
        icon = icon("brazilian-real-sign")
      ),
      valueBox(
        width = 4,
        value = textOutput("median_price"),
        subtitle = "Median Price",
        icon = icon("brazilian-real-sign")
      ),
      valueBox(
        width = 4,
        value = textOutput("listings_count"),
        subtitle = "Total Listings",
        icon = icon("list")
      )
    ),
    fluidRow(
      box(
        width = 4,
        title = "Histogram",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        plotOutput("distPlot")
      ),
      box(
        width = 4,
        title = "Density Plot",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        plotOutput("densityPlot")
      ),
      box(
        width = 4,
        title = "Box Plot",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        plotOutput("boxPlot")
      )
    ),
    fluidRow(
      box(
        width = 12,
        title = "Price vs availability on next 365 days",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        plotOutput("scatterplot")
      )
    ),
    fluidRow(
      box(
        width = 12,
        title = "Airbnb listings at Rio de Janeiro",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        DT::DTOutput("listingsTable")
      )
    )
)
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
   
   output$scatterplot <- renderPlot({
     x <- listings$price
     y <- listings$availability_365
     
     ggplot(data.frame(x, y)) +
       aes(x = x, y = y) +
       geom_point() +
       labs(x = "Price", y = "Availability on the next 365 days", title = "Price vs Availability") + 
       theme_minimal()  # Adiciona um tema para melhor visualização
   })

   filtered_listings <- reactive({
     if (length(input$neighbourhood_filter) == 0) {
       listings
     } else {
       listings %>% 
         filter(neighbourhood %in% input$neighbourhood_filter)
     }
   })
   output$listingsTable <- DT::renderDataTable({
     DT::datatable(
       filtered_listings() %>%
         select(name, neighbourhood, room_type, price, number_of_reviews, availability_365, number_of_reviews_ltm, calculated_host_listings_count),
       options = list(pageLength = 10, autoWidth = TRUE),
       rownames = FALSE
     )
   })
 }


# Run the application
shinyApp(ui, server, options = list(display.mode = "showcase"))