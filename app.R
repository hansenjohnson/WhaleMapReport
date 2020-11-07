## app.R ##
# app for generating WhaleMap summary reports

# setup -------------------------------------------------------------------

library(shiny)
library(shinyjs)
library(tidyverse)

# path to report template file (`dfo_summary-template.Rmd`)
template_file = "dfo_summary-template.Rmd"

# read in source code
source('functions.R')

# read in password
load('password.rda')

# app ---------------------------------------------------------------------

ui = fluidPage(
  shinyjs::useShinyjs(),
  titlePanel(title = 'WhaleMap Summary Report'),
  dateInput("date", label = 'Choose date:', 
            max = Sys.Date(), 
            value = Sys.Date()-1),
  selectInput("type", 
              label = 'Choose report type:', 
              choices = c('daily', 'daily-extended', 'weekly'), 
              selected = 'daily',
              multiple = FALSE),
  passwordInput("password", "Password:"),
  shinyjs::hidden(downloadButton("download","Download report"))
)

server = function(input, output, session) {
  
  # render report and download
  observe({
    output$download <- downloadHandler(
      filename = function(){
        paste0(as.Date(input$date), '_WhaleMap_',input$type,'_summary.pdf')},
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
        obs = readRDS('../WhaleMap/data/processed/observations.rds') %>%
          dplyr::filter(date >= t1 & date <= t2) %>%
          dplyr::filter(score %in% c('definite acoustic', 'definite visual')) %>%
          subset_canadian()
        trk = readRDS('../WhaleMap/data/processed/tracks.rds') %>%
          dplyr::filter(date >= t1 & date <= t2) %>%
          subset_canadian()
        
        # copy template to a temporary directory to avoid permissions issues 
        tempReport = file.path(tempdir(), template_file)
        file.copy(template_file, tempReport, overwrite = TRUE)
        
        # render report
        rmarkdown::render(input = tempReport,
                          output_file = file,
                          params = list(
                            t1 = t1,
                            t2 = t2,
                            obs = obs,
                            trk = trk))
      }
    )
  })
  
  # enable/disable download based on password
  observeEvent(input$password, {
    if(input$password == password){
      shinyjs::show("download")
    } else {
      shinyjs::hide("download")
    }
  })
}

shinyApp(ui, server)
