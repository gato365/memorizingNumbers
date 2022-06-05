#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Numbers Shown"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)
      
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      selectInput(inputId = 'selectRow',
                  label = 'Select Row',
                  choices = c('1','2','3')),
      
      textAreaInput(inputId = 'input_text',
                    label = 'Place Numbers Here:',
                    value = "Numbers", 
                    width = '400px',
                    height = '200px'),
      verbatimTextOutput(outputId ='output_text', placeholder = FALSE)
      
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  output$output_text<- renderText({ 
    
    if(str_detect(input$input_text,'\n')){
      info_given = str_split(input$input_text,'\n')[[1]]
      info_given[as.numeric(input$selectRow)]
    }
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
