# Load the necessary library
library(shiny)

# Define the UI
ui <- fluidPage(
  actionButton("start", "Start Timer"),
  actionButton("stop", "Stop Timer"),
  actionButton("hide", "Hide/Show Timer"),
  textOutput("timer")
)

# Define the server logic
server <- function(input, output, session) {
  values <- reactiveValues(timer = 0, showTimer = TRUE, running = FALSE)
  
  observeEvent(input$start, {
    values$timer <- 0
    values$showTimer <- TRUE
    values$running <- TRUE
  })
  
  observeEvent(input$stop, {
    values$running <- FALSE
  })
  
  observeEvent(input$hide, {
    values$showTimer <- !values$showTimer
  })
  
  observe({
    if (values$running) {
      values$timer <- isolate(values$timer) + 1
      invalidateLater(1000, session)
    }
  })
  
  output$timer <- renderText({
    if (values$showTimer) {
      paste("Timer:", values$timer)
    } else {
      ""
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
