# QS trend analysis - sidebar
library(tidyverse)
library(shiny)
library(shinydashboard)
library(shinyFiles)
library(shinyjs)
library(DT)
library(plotly)

# Import global variables and functions
source("./global.R")

# UI ----
ui <- fluidPage(
  # App title
  titlePanel(title = "QS Trend Analysis (v0.0.6)"),
  
  fluidRow(
    column(3,
           source(file.path("ui", "sidebar.R"), local = TRUE)$value),
    
    column(8,
           tabsetPanel(id = "tabs",
                       source(file.path("ui", "tab_main.R"),  local = TRUE)$value,
                       source(file.path("ui", "tab_db.R"),  local = TRUE)$value,
                       source(file.path("ui", "tab_plot.R"),  local = TRUE)$value
           )
    )
  )
)

# All functionality in the back-end
# Server ----
server <- shinyServer(function(input, output, session) {
  # Check presence qc file ----
  output$qc_present <- reactive({
    if(file.exists("./../qc.csv")) {
      qc <<- read_csv("./../qc.csv",
                      col_types = list(col_character(), col_datetime(format = ""),
                                       col_double(), col_double()))
      return(T)
    } else {
      qc <<- NULL
      return(F)
    }
  })
  
  outputOptions(output, "qc_present", suspendWhenHidden=FALSE)
  
  # Import server tabs ----
  source(file.path("server", "server_main.R"),  local = TRUE)$value
  source(file.path("server", "server_db.R"),  local = TRUE)$value
  source(file.path("server", "server_plot.R"),  local = TRUE)$value
})

# Run application 
shinyApp(ui = ui, server = server)
