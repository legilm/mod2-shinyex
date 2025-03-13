# Shiny app for Airbnb listings in Rio de Janeiro
library(shiny)
library(tidyverse)
library(shinydashboard)
library(shinyWidgets)
library(DT)

# Load data
listings <- rio::import("data/final_dt.csv")

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
                selected = unique(listings$neighbourhood)[1],
                options = pickerOptions(
                  actionsBox = TRUE
                ), 
                multiple = TRUE
    )
  ),
  dashboardBody(
    fluidRow(
      bs4ValueBoxOutput("mean_price"),
      bs4ValueBoxOutput("median_price"),
      bs4ValueBoxOutput("listings_count")
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
 

# Value Boxes -------------------------------------------------------------

 
 output$mean_price <- renderValueBox({
   valueBox(
     width = 4,
     value = paste("R$", round(mean(listings$price), 2)),
     subtitle = "Mean Price",
     icon = icon("brazilian-real-sign")
   )
 })
 
 output$median_price <- renderValueBox({
   valueBox(
     width = 4,
     value = paste("R$", round(median(listings$price), 2)),
     subtitle = "Median Price",
     icon = icon("brazilian-real-sign")
   )
 })
 
 output$listings_count <- renderValueBox({
   valueBox(
     width = 4,
     value = nrow(listings),
     subtitle = "Total Listings",
     icon = icon("list")
   )
 })
 
 

# Rest --------------------------------------------------------------------

 
   
   output$distPlot <- renderPlot({
     x <- listings$price
     bins <- seq(min(x), max(x), length.out = input$bins + 1)
     
     ggplot(data.frame(x), aes(x)) +
       geom_histogram(breaks = bins, color = "white", fill = input$color) +
       labs(x = "Price of the listings, in Brazilian Reais (R$)", y = "Frequency") +
       theme_choice()
   })
   
   output$densityPlot <- renderPlot({
     x <- listings$price

     ggplot(data.frame(x), aes(x)) +
       geom_density(alpha = 0.5, fill = input$color) +
       labs(title = "Density Plot of Prices, in Brazilian Reais (R$)", x = "Prices (R$)", y = "Density") +
       theme_choice()
   })
   
   output$boxPlot <- renderPlot({
     x <- listings$price
     
     ggplot(data.frame(x), aes(y = x)) +
       geom_boxplot(color = "black", fill = input$color) +
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
    req(input$neighbourhood_filter)
     listings %>% 
       filter(neighbourhood %in% input$neighbourhood_filter) |> 
       select(name, neighbourhood, room_type, price, number_of_reviews, availability_365, number_of_reviews_ltm, calculated_host_listings_count)
   })
   
   
   output$listingsTable <- DT::renderDataTable({
     DT::datatable(
       filtered_listings(),
       options = list(pageLength = 10, autoWidth = TRUE),
       rownames = FALSE
     )
   })
 }


# Run the application
shinyApp(ui, server)