#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(lubridate)


setwd("~/Important_Files/Life/01_thoughts_beliefs/01_enhancing_self/memorizingNumbers")
source('bring_in_e.R')

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Numbers Shown"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      selectInput(inputId = 'selectedGroup',
                  label = 'Select Grouping:',
                  choices = c('A','B','C','D','E')),
      
      textAreaInput(inputId = 'input_text',
                    label = 'Place Numbers Here:',
                    value = "Numbers", 
                    width = '400px',
                    height = '200px'),
      verbatimTextOutput(outputId ='output_text', placeholder = FALSE),
      actionButton(inputId = 'button',label = 'Evaluate')
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  
  
  output$output_text<- renderText({ 
    
    
    tmp_df = df %>%
      filter(labeled == input$selectedGroup)
    
    if(str_detect(input$input_text,'\n')){
      info_given = str_split(input$input_text,'\n')[[1]]
      info_given[1]
    }
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
