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
library(kableExtra)


# setwd("~/Important_Files/Life/01_thoughts_beliefs/01_enhancing_self/memorizingNumbers")
source('bring_in_e.R')

# mod_df = df %>% 
#   mutate(
#     ordered = paste0('\'',c(rep(1:5,5),1,2,3),'\''),
#     Merged = paste0('\'',as.character(Merged),'\''),
#     labeled =  paste0('\'',as.character(labeled),'\''),
#     new_var = paste0(
#       '{ \n trueString: ',Merged,',\n',
#       'labeled: ',labeled,',\n',
#       'ordered: ',ordered,',\n',
#       '},'
#       
#       
#     ))
# 
# objText = mod_df$new_var
# write(objText,'e_as_object.txt')


##---------------------------------
## Name: getSolutionLine
## Purpose: get row number based on selected row
## Input: data frame of numbers and row number
## Output: string
##---------------------------------
getSolutionLine = function(df,number){
  realSolution = str_extract_all(pull(df[number,1],Merged), boundary("character"))[[1]] %>%
    str_remove('\\.') %>%
    str_subset( ".+")
  return(realSolution)
  
}


##----------------------------------------
## Name: displayGrade
## Purpose: Display user's grade regarding my solution
## Input: number, user solution, data frame
## Output: Statement about correctness
##----------------------------------------
displayGrade = function(number,emansSolutionLine,df){
  
  realsSolutionLine = getSolutionLine(df,number)
  
  emansSolutionLine = str_extract_all(emansSolutionLine, boundary("character"))[[1]]
  
  ## Check
  emans_len = length(emansSolutionLine)
  reals_len = length(realsSolutionLine)
  
  if( sum(emansSolutionLine != realsSolutionLine[1:emans_len]) > 0 ){
    percentWrong = paste0('<font color=\"#FF0000\"><b>Your Current % incorrect: ', 
                          round((sum(emansSolutionLine != realsSolutionLine[1:emans_len])/ length(1:emans_len)),3) * 100, ' %',tags$br(),
                          '</b></font>')
  } else {
    
    percentWrong = ''
  }
  
  
  ## Solution
  grading = paste0('Line ',number,':', tags$br(),
                   'Your Current % correct: ', 
                   round(sum(emansSolutionLine == realsSolutionLine[1:emans_len])/ reals_len,3) * 100, ' %',tags$br(),
                   percentWrong,
                   'You have ',reals_len - emans_len ,' more numbers to enter'
  )
  
  
  return(grading)
  
}

##----------------------------------------
## Name: determineNumberLine
## Purpose: Show the current line
## Input: number of breaks, user solution, data frame
## Output: Statement about correctness based on line
##----------------------------------------
determineNumberLine = function(number_breaks,info_given,tmp_df){
  emansSolutionLine = info_given[number_breaks+1]
  grading = displayGrade(number_breaks+1,emansSolutionLine,tmp_df)
  return(grading)
}



# # Define UI for application that draws a histogram
# ui <- fluidPage(
#   
#   # Application title
#   titlePanel("Numbers Shown"),
#   
#   # Show a plot of the generated distribution
#   mainPanel(
#     checkboxInput('show_table',label = 'Show current rows numbers', value = FALSE),
#     
#     selectInput(inputId = 'type_of_test',
#                 label = 'What type of test is being administered?',
#                 choices = c('Weekday','Weekend')),
#     
#     conditionalPanel(
#       condition = "input.type_of_test == 'Weekday'",
#       selectInput(inputId = 'selectedGroup',
#                   label = 'Select Grouping:',
#                   choices = c('A','B','C','D','E')),
#       actionButton("start", "Start Timer"),
#       actionButton("stop", "Stop Timer"),
#       actionButton("hide", "Hide/Show Timer"),
#       textOutput("timer")
#       
#     ),
#     
#     
#     
#     
#     
#     
#   ),
#   
#   
#   tags$style("#input_text {font-size:13px;}"),
#   textAreaInput(inputId = 'input_text',
#                 label = 'Place Numbers Here:',
#                 value = "", 
#                 width = '450px',
#                 height = '200px',
#                 resize = 'horizontal'),
#   htmlOutput('output_text'),
#   tableOutput("numbers_kable")
#   # actionButton(inputId = 'button',label = 'Evaluate')
# )


ui <- fluidPage(
  
  # Application title
  titlePanel("Numbers Shown", windowTitle = "Numbers Shown"),
  
  # Show a plot of the generated distribution
  mainPanel(
    tags$style(HTML("
      .skin-blue .main-header .logo { background-color: #3c8dbc; }
      .skin-blue .main-header .navbar { background-color: #3c8dbc; }
      .skin-blue .main-header .navbar .sidebar-toggle:hover { background-color: #367fa9; }
      .skin-blue .main-header .navbar .nav > li > a { color: #ffffff; }
      .skin-blue .main-header .navbar .nav > li > a:hover { background: #367fa9; }
      .skin-blue .main-header .navbar .nav > li > a:focus { background: #367fa9; }
      .skin-blue .main-header .logo:hover { background-color: #367fa9; }
      .skin-blue .main-sidebar { background-color: #222d32; }
      .skin-blue .sidebar a { color: #b8c7ce; }
      .skin-blue .sidebar .sidebar-menu > li.header { color: #4b646f; background: #1a2226; }
      .skin-blue .sidebar .sidebar-menu > li > a:hover { color: #ffffff; background: #1e282c; }
      .skin-blue .sidebar .sidebar-menu > li > a:focus { color: #ffffff; background: #1e282c; }
      .skin-blue .sidebar .sidebar-menu > li.active > a { color: #ffffff; background: #1e282c; }
      .skin-blue .sidebar .sidebar-menu > li.active > a:hover { color: #ffffff; background: #1e282c; }
      .skin-blue .sidebar .sidebar-menu > li.active > a:focus { color: #ffffff; background: #1e282c; }
      .skin-blue .content-wrapper, .skin-blue .main-footer { background-color: #ffffff; }
      .skin-blue .wrapper, .skin-blue .main-sidebar, .skin-blue .left-side { background-color: #222d32; }
      .skin-blue .content-wrapper, .skin-blue .main-footer, .skin-blue .main-footer .container { background-color: #ffffff; }
    ")),
    
    checkboxInput('show_table',label = 'Show current rows numbers', value = FALSE),
    
    selectInput(inputId = 'type_of_test',
                label = 'What type of test is being administered?',
                choices = c('Weekday','Weekend')),
    
    conditionalPanel(
      condition = "input.type_of_test == 'Weekday'",
      selectInput(inputId = 'selectedGroup',
                  label = 'Select Grouping:',
                  choices = c('A','B','C','D','E')),
      actionButton("start", "Start Timer", class = "btn btn-success"),
      actionButton("stop", "Stop Timer", class = "btn btn-danger"),
      actionButton("hide", "Hide/Show Timer", class = "btn btn-info"),
      textOutput("timer")
      
    ),
    
    tags$style("#input_text {font-size:13px;}"),
    textAreaInput(inputId = 'input_text',
                  label = 'Place Numbers Here:',
                  value = "", 
                  width = '450px',
                  height = '200px',
                  resize = 'horizontal'),
    htmlOutput('output_text'),
    tableOutput("numbers_kable")
  )
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  ##----------------------------
  ## TIme element:
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
  
  
  
  ##----------------------------
  
  
  output$output_text<- renderUI({ 
    
    ## Select row from user
    
    if(input$type_of_test == 'Weekday'){
      tmp_df = df %>%
        filter(labeled == input$selectedGroup)
    } else {
      tmp_df = df
    }
    
    info_given = str_split(input$input_text,'\n')[[1]]
    
    
    
    number_breaks = sum(str_count(input$input_text,'\n'))
    info = determineNumberLine(number_breaks,info_given,tmp_df)
    
    
    
    
    
    
    ## Display Grade
    HTML(info)
    
  })
  
  output$text <- renderText({ input$txt })
  
  
  
  output$numbers_kable <- function() {
    
    if(input$show_table == TRUE){
      
      df %>%
        filter(labeled == input$selectedGroup) %>% 
        filter(row_number() == sum(str_count(input$input_text,'\n'))+1 ) %>% 
        select(-labeled) %>% 
        kable("html") %>%
        kable_styling("striped", full_width = F) 
    }
    
  }
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
