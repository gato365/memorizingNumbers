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
  
  
  
  ## Solution
  grading = paste0('Line ',number,':', tags$br(),
                   'Your Current % correct: ', 
                   round(sum(emansSolutionLine == realsSolutionLine[1:emans_len])/ reals_len,3) * 100, ' %',tags$br(),
                   'Your Current % incorrect: ', 
                   round(1 - (sum(emansSolutionLine == realsSolutionLine[1:emans_len])/ reals_len),3) * 100, ' %',tags$br(),
                   'You have ',reals_len - emans_len ,' more numbers to enter'
  )
  
  
  return(grading)
  
}


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
                    value = "", 
                    width = '400px',
                    height = '200px'),
      htmlOutput('output_text'),
      # verbatimTextOutput(outputId ='output_text', placeholder = FALSE),
      actionButton(inputId = 'button',label = 'Evaluate')
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  
  
  output$output_text<- renderUI({ 
    
    ## Select row from user
    tmp_df = df %>%
      filter(labeled == input$selectedGroup)
    
    info_given = str_split(input$input_text,'\n')[[1]]
    
    
    
    
    if(sum(str_detect(input$input_text,'\n')) >= 0){            ## 1st Line
      emansSolutionLine = info_given[1]
      grading1 = displayGrade(1,emansSolutionLine,tmp_df)
      info = grading1
    }
    
    if(sum(str_detect(input$input_text,'\n')) == 1 ){    ## 2nd Line
      emansSolutionLine = info_given[2]
      grading2 = displayGrade(2,emansSolutionLine,tmp_df)
      info = paste0(info, tags$br(), grading2)
    }
    
    if(sum(str_detect(input$input_text,'\n')) == 2){    ## 3rd Line
      emansSolutionLine = info_given[3]
      grading3 = displayGrade(3,emansSolutionLine,tmp_df)
      info = paste0(info, '\n','Line 3: \n', grading3)
    }
    
    if(sum(str_detect(input$input_text,'\n')) == 3){     ## 4th Line
      emansSolutionLine = info_given[4]
      grading4 = displayGrade(4,emansSolutionLine,tmp_df)
      
      info = paste0(info, '\n','Line 4: \n', grading4)
    }
    
    if(sum(str_detect(input$input_text,'\n')) == 4){     ## 5th Line
      emansSolutionLine = info_given[5]
      grading5 = displayGrade(5,emansSolutionLine,tmp_df)
      info = paste0(info, '\n','Line 5: \n', grading5)
    }
    
    
    ## Display Grade
    HTML(info)
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
