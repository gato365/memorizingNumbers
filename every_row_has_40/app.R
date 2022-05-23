library(shiny)

library(readxl)

library(tidyverse)

library(lubridate)


setwd("~/Important_Files/Life/01_thoughts_self/01_enhancing_self/memorizingNumbers")
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
      # verbatimTextOutput("evaluation"),
      # radioButtons('answer','Show Answer', c('Nothing','Answer','Location'), selected = 'Nothing'),
      # htmlOutput("eval_answer")
      
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  ##----------------------------
  ##
  ##
  ## Evaluating Just 1 Row of 40
  ##
  ##
  ##----------------------------
  output$evaluation <- renderPrint({ 
    
    info = input$which_set
    number_row = as.numeric(str_remove(info,'set-'))
    
    ## Get My answer
    emans_solution = str_extract_all(input$solution_number, boundary("character"))[[1]]
    
    
    
    tmp_df = df %>% 
      slice(number_row)
    
    ## The Solution
    reals_solution = str_extract_all(pull(tmp_df[number_row,1],Merged), boundary("character"))[[1]] %>% 
      str_remove('\\.') %>% 
      str_subset( ".+")
    
    numbers_tested = length(reals_solution)
    ## Check
    paste0(round(sum(emans_solution == reals_solution)/ numbers_tested,3) * 100, ' %')
  })
  
  ## Answer Box
  output$eval_answer <- renderUI({
    
    
    # ## Get My answer
    # emans_solution = str_extract_all(input$solution_number, boundary("character"))[[1]]
    # ## The Solution
    # reals_solution = str_extract_all(select_set_fun(input$which_set), boundary("character"))[[1]] %>% 
    #   str_remove('\\.') %>% 
    #   str_subset( ".+")
    # 
    # 
    # ## User wants to see answer
    # if(input$answer == 'Answer'){
    #   HTML(paste(reals_solution,collapse = '')) 
    # } else if(input$answer == 'Location') {
    #   HTML(paste(ifelse(emans_solution == reals_solution,1,0),collapse = ''))
    # } else if(input$answer == 'Nothing'){
    #   HTML('')
    # }
    
    
    # info = input$which_set
    # number_row = as.numeric(str_remove(info,'set-'))
    # tmp_df = df %>% 
    #   # filter(labeled == input$set_combination)
    #   slice(number_row)
    # 
    # ## Get My answer
    # emans_solution = str_extract_all(input$solution_number, boundary("character"))[[1]]
    # 
    # 
    # ## The Solution
    # reals_solution = str_extract_all(pull(tmp_df[info,1],Merged), boundary("character"))[[1]] %>% 
    #   str_remove('\\.') %>% 
    #   str_subset( ".+")
    # 
    # ## User wants to see answer
    # if(input$answer1 == 'Answer'){
    #   HTML(paste(reals_solution,collapse = '')) 
    # } else if(input$answer1 == 'Location') {
    #   HTML(paste(ifelse(emans_solution == reals_solution,1,0),collapse = ''))
    # } else if(input$answer1 == 'Nothing'){
    #   HTML('')
    # }
    
    cat('s')
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
