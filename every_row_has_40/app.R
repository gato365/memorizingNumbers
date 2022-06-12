library(shiny)
library(readxl)
library(tidyverse)
library(lubridate)
library(kableExtra)

setwd("~/Important_Files/Life/01_thoughts_beliefs/01_enhancing_self/memorizingNumbers")
source('bring_in_e.R')


df = df %>% 
  mutate(row_number = 1:n())

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("How many Numbers are known of e are memorized?"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      
      selectInput('which_set','Select Set to Test',choices = e_number_df$set),
      checkboxInput('show_table',label = 'Show the all numbers', value = FALSE)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      ## Checking 1 set
      textInput("solution_number", label = h2("Set Numbers"), value = "",width = "400px"),
      htmlOutput("evaluation"),
      shiny::column(4, radioButtons('answer','Show Answer', c('Nothing','Answer','Location','Show Hint'), selected = 'Nothing')),
      htmlOutput("eval_answer"),
      tableOutput("numbers_kable")
      
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  ##----------------------------
  ##
  ##
  ## Evaluating Just 1 Row of 28 Year: 2022
  ##
  ##
  ##----------------------------
  output$evaluation <- renderUI({
    
    ## Get Selected Row
    number_row = as.numeric(str_remove(input$which_set,'set-'))
    
    ## Get My answer
    emans_solution = str_extract_all(input$solution_number, boundary("character"))[[1]]
    
    
    ## Get Solution Row
    tmp_df = df %>%
      slice(number_row)
    
    ## Format Solution Row
    reals_solution = str_extract_all(pull(tmp_df[,1],Merged), boundary("character"))[[1]] %>%
      str_remove('\\.') %>%
      str_subset( ".+")
    
    
    ## Check
    emans_len = length(emans_solution)
    correct_len = length(reals_solution)
    
    
    if(emans_len == 0){
      waiting = paste0('Waiting, you have ',correct_len,' numbers to input.')
      
      
      HTML(waiting)
    } else {
      grading = paste0('Your Current % correct: ', 
                       round(sum(emans_solution == reals_solution[1:emans_len])/ correct_len,3) * 100, ' %',tags$br(),
                       'You have ',correct_len - emans_len ,' more numbers to enter'
      )
      
      HTML(grading)
    }
    
    
    # 
  })
  
  
  
  
  
  
  
  
  ## Answer Box
  output$eval_answer <- renderUI({
    
    
    
    ## Get Selected Row
    number_row = as.numeric(str_remove(input$which_set,'set-'))
    
    ## Get My answer
    emans_solution = str_extract_all(input$solution_number, boundary("character"))[[1]]
    
    
    
    ## Get Solution Row
    tmp_df = df %>%
      slice(number_row)
    
    
    
    
    ## Format Solution Row
    reals_solution = str_extract_all(pull(tmp_df[,1],Merged), boundary("character"))[[1]] %>%
      str_remove('\\.') %>%
      str_subset( ".+")
    
    
    
    
    ## User wants to see answer
    if(input$answer == 'Answer'){
      HTML(paste(reals_solution,collapse = ''))
    } else if(input$answer == 'Location') {
      HTML(paste(ifelse(emans_solution == reals_solution,1,0),collapse = ''))
    } else if(input$answer == 'Nothing'){
      HTML('')
    } else if(input$answer == 'Show Hint'){
      HTML(reals_solution[1:5])
      
    }
    
    
    
  })
  
  output$numbers_kable <- function() {
    
    if(input$show_table == TRUE){
      
      df %>% 
        select(-labeled) %>% 
        kable("html") %>%
        kable_styling("striped", full_width = F) 
    }
    
  }
  
  
  

}

# Run the application 
shinyApp(ui = ui, server = server)
