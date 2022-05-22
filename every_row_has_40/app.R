library(shiny)

library(readxl)

library(tidyverse)

library(lubridate)

setwd("C:/Users/james/OneDrive/Documents/Important_Files/Life/01_thoughts_self/01_enhancing_self/memorizingNumbers/every_row_has_40/")
source('bring_in_e.R')


select_set_fun <- function(specified_set) {
  ## The Solution
  value_to_test = e_number_df %>% 
    filter(set == specified_set) %>% 
    select(-set) %>% 
    unite('Merged', `col-1`:`col-4`,remove =FALSE,sep = '') %>% 
    mutate(Merged = str_remove_all(Merged,'\'')) %>% 
    pull(Merged)
  return(value_to_test)
}





# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("How many Numbers are known of e are memorized?"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      
      selectInput('which_set','Select Set to Test',choices = e_number_df$set),
 
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      ## Checking 1 set
      textInput("solution_number", label = h2("Set Numbers"), value = "",width = "400px"),
      verbatimTextOutput("evaluation"),
      radioButtons('answer','Show Answer', c('Nothing','Answer','Location'), selected = 'Nothing'),
      htmlOutput("eval_answer")
      
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
