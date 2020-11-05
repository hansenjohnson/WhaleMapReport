## app.R ##
# app for generating WhaleMap summary reports

# setup -------------------------------------------------------------------

library(shiny)
library(tidyverse)

# path to report template file (`dfo_summary-template.Rmd`)
template_file = normalizePath("dfo_summary-template.Rmd")

# read in source code
source(normalizePath('functions.R'))

# read in password
load('password.rda')

# app ---------------------------------------------------------------------

ui = fluidPage(titlePanel(title = 'WhaleMap Summary Report'),
               dateInput("date", label = 'Choose date:', 
                         max = Sys.Date(), 
                         value = Sys.Date()-1),
               selectInput("type", 
                           label = 'Choose report type:', 
                           choices = c('daily' = 0, 'daily-extended' = 3, 'weekly' = 6), 
                           selected = 'daily',
                           multiple = FALSE),
               passwordInput("password", "Password:"),
               actionButton("check", 'Check password'),
               uiOutput("download")
)

server = function(input, output) {
  
  observeEvent(input$check, {
    
    if(input$password == password){
      showNotification('Password correct! You may now download the report :)', type = 'message')
      output$download <- renderUI({downloadButton("report", "Download report")})
    } else {
      showNotification('Incorrect password. Please contact hansen.johnson@dal.ca for access', type = 'warning')
    }
    
  })
  
  output$report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report.pdf",
    content = function(file) {
      
      # Copy the report file to a temporary directory before processing it
      tempReport <- file.path(tempdir(), basename(template_file))
      file.copy(template_file, tempReport, overwrite = TRUE)
      
      # extract date limits from inputs
      t2 = as.Date(input$date)
      t1 = t2-as.numeric(input$type)
      
      # read and subset data
      obs = readRDS('../WhaleMap/data/processed/observations.rds') %>% 
        dplyr::filter(date >= t1 & date <= t2) %>%
        dplyr::filter(score %in% c('definite acoustic', 'definite visual')) %>% 
        subset_canadian()
      trk = readRDS('../WhaleMap/data/processed/tracks.rds') %>% 
        dplyr::filter(date >= t1 & date <= t2) %>% 
        subset_canadian()
      
      # render report
      rmarkdown::render(input = tempReport, 
                        output_file = file,
                        params = list(
                          t1 = t1,
                          t2 = t2,
                          obs = obs,
                          trk = trk),
                        envir = new.env(parent = globalenv()))
    }
  )
}

shinyApp(ui, server)
