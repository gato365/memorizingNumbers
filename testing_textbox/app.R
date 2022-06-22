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


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Numbers Shown"),
  
  # Show a plot of the generated distribution
  mainPanel(
    checkboxInput('show_table',label = 'Show current rows numbers', value = FALSE),
    selectInput(inputId = 'selectedGroup',
                label = 'Select Grouping:',
                choices = c('A','B','C','D','E')),
    
    textAreaInput(inputId = 'input_text',
                  label = 'Place Numbers Here:',
                  value = "", 
                  width = '400px',
                  height = '200px'),
    htmlOutput('output_text'),
    tableOutput("numbers_kable")
    # actionButton(inputId = 'button',label = 'Evaluate')
  )
  
  
 
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  
  
  output$output_text<- renderUI({ 
    
    ## Select row from user
    tmp_df = df %>%
      filter(labeled == input$selectedGroup)
    
    info_given = str_split(input$input_text,'\n')[[1]]
    
    
    info = ''
    
    if(sum(str_count(input$input_text,'\n')) == 0){            ## 1st Line
      emansSolutionLine = info_given[1]
      grading1 = displayGrade(1,emansSolutionLine,tmp_df)
      info = grading1
    }
    
    if(sum(str_count(input$input_text,'\n')) == 1 ){    ## 2nd Line
      emansSolutionLine = info_given[2]
      grading2 = displayGrade(2,emansSolutionLine,tmp_df)
      info = paste0(info, tags$br(), grading2)
    } 
    
    if(sum(str_count(input$input_text,'\n')) == 2){    ## 3rd Line
      emansSolutionLine = info_given[3]
      grading3 = displayGrade(3,emansSolutionLine,tmp_df)
      info = paste0(info,tags$br(), grading3)
    }
    
    if(sum(str_count(input$input_text,'\n')) == 3){     ## 4th Line
      emansSolutionLine = info_given[4]
      grading4 = displayGrade(4,emansSolutionLine,tmp_df)
      info = paste0(info, tags$br(), grading4)
    }
    
    if(sum(str_count(input$input_text,'\n')) == 4){     ## 5th Line
      emansSolutionLine = info_given[5]
      grading5 = displayGrade(5,emansSolutionLine,tmp_df)
      info = paste0(info, tags$br(), grading5)
    }
    
  
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
