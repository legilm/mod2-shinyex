library(shiny)
library(bs4Dash)
library(dplyr)

ui <- dashboardPage( #define UI
   dashboardHeader(title = "Iris Species Dashboard"),
   sidebar = dashboardSidebar(
    helpText("This dashboard filters the Iris dataset based on species and sepal length."),
    selectInput("species", 
                label = "Choose a specie:", 
                choices = unique(iris$Species),
                selected = unique(iris$Species)[1]
    ),
    sliderInput("sepal_length",
                label = "Sepal length:",
                min = min(iris$Sepal.Length),
                max = max(iris$Sepal.Length),
                value = c(min(iris$Sepal.Length), max(iris$Sepal.Length)),
                step = 0.1
                ),
    actionButton("filter_data",
                 label = "Filter data"
                 )
   ),
   body = dashboardBody(
     fluidRow(
       box(
         title = "Filtered Iris Dataset",
                width = 12,
                tableOutput("iris_table")
              )
       )
     )
   )


server <- function(input, output, session) {
  filtered_by_species <- reactive({
    iris %>%
      filter(Species == input$species)
  })
  
  observe({
    cat("Selected species: ", input$species, "\n")
  })
  
  filtered_data <- eventReactive(input$filter_data, {
    req(input$sepal_length)
    filtered_by_species() %>%
      filter(Sepal.Length >= input$sepal_length[1] & 
               Sepal.Length <= input$sepal_length[2])
  },
  )
  
  
  output$iris_table <- renderTable({
  filtered_data()
  })
}

# Run the application
shinyApp(ui, server, options = list(display.mode = "showcase"))