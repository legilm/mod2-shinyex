library(bs4Dash)
library(shiny)

ui <- dashboardPage(
  dashboardHeader(title = "Score dashboard"),
  dashboardSidebar(
    sidebarMenu(
     textInput("name", "Enter your name here:", ""),
     actionButton("add_score", "Add score"),
     actionButton("reset", "Reser score")
    )
  ),
  dashboardBody(
    
    box(
      title = "Current user and score",
      status = "primary",
      solidHeader = FALSE,
      width = 12,
      textOutput("current_user_and_score")
    )
  )
)

server <- function(input, output) {
  values <- reactiveValues(
    name = "",
    score = 0
  )
  
  observeEvent(input$add_score, {
    values$score <- values$score + 1
  })
  
  observeEvent(input$reset, {
    values$score <- 0
  })
    
    output$current_user_and_score <- renderText({
      paste("User:", input$name, "- Score:", values$score)
    })
    
}

# Run the application
shinyApp(ui, server, options = list(display.mode = "showcase"))