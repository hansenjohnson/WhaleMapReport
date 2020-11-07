## app.R ##
# app for generating WhaleMap summary reports

# setup -------------------------------------------------------------------

library(shiny)
library(tidyverse)

# path to report template file (`dfo_summary-template.Rmd`)
template_file = "dfo_summary-template.Rmd"

# read in source code
source('functions.R')

# read in password
load('password.rda')

# app ---------------------------------------------------------------------

ui = fluidPage(titlePanel(title = 'WhaleMap Summary Report'),
               dateInput("date", label = 'Choose date:', 
                         max = Sys.Date(), 
                         value = Sys.Date()-1),
               selectInput("type", 
                           label = 'Choose report type:', 
                           choices = c('daily', 'daily-extended', 'weekly'), 
                           selected = 'daily',
                           multiple = FALSE),
               passwordInput("password", "Password:"),
               actionButton("check", 'Check password'),
               # uiOutput("download")
               # downloadButton("report", "Download report")
)

server = function(input, output) {
  
  # read in data
  min_date = Sys.Date()-6
  obs = readRDS('../WhaleMap/data/processed/observations.rds') %>% filter(date>=min_date)
  trk = readRDS('../WhaleMap/data/processed/tracks.rds') %>% filter(date>=min_date)
  
  # # check password
  # observeEvent(input$check, {
  #   
  #   if(input$password == password){
  #     showNotification('Password correct! You may now download the report :)', type = 'message')
  #     output$download <- renderUI({downloadButton("report", "Download report")})
  #   } else {
  #     showNotification('Incorrect password. Please contact hansen.johnson@dal.ca for access', type = 'warning')
  #   }
  #   
  # })
  
  # download
  output$report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    # filename = "report.csv",
    filename = paste0(as.Date(input$date), '_WhaleMap_',input$type,'_summary.csv'),
    
    content = function(file) {
      
      # define report duration
      d = switch(input$type,
                 'daily' = 0, 
                 'daily-extended' = 3,
                 'weekly' = 6)
      
      # define date limits
      t2 = as.Date(input$date)
      t1 = t2-d
      
      # read and subset data
      obs = obs %>%
        dplyr::filter(date >= t1 & date <= t2) %>%
        dplyr::filter(score %in% c('definite acoustic', 'definite visual')) %>%
        subset_canadian()
      
      write.csv(obs,file,row.names = FALSE)
      # trk = readRDS('../WhaleMap/data/processed/tracks.rds') %>%
      #   dplyr::filter(date >= t1 & date <= t2) %>%
      #   subset_canadian()
      
      # # render report
      # rmarkdown::render(input = tempReport, 
      #                   output_file = file,
      #                   params = list(
      #                     t1 = t1,
      #                     t2 = t2,
      #                     obs = obs,
      #                     trk = trk),
      #                   envir = new.env(parent = globalenv()))
    }
  )
}

shinyApp(ui, server)
