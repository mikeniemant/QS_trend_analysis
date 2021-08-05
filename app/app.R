# QS trend analysis - sidebar
library(shiny)
library(shinydashboard)
library(shinyFiles)
library(tidyverse) # TODO: maybe extract the relevant packages
library(DT)
library(plotly)
# library(shinyWidgets)

# Import global variables and functions
source("./global.R")

# UI ----
ui <- fluidPage(
  # App title
  titlePanel(title = "QS trend analysis | V 0.0.2"),
  
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
  
  # Check presence database ----
  output$db_present <- reactive({
    if(file.exists(DB_PATH)) {
      # print("DB exists")
      db <<- read_csv(DB_PATH)

      # Render files df
      output$db_files <- renderDataTable(db %>% 
                                           select(date, exp_name) %>% 
                                           distinct(), 
                                         options = list(# dom = 't',
                                           # paging = TRUE,
                                           pageLength = 5))
      
      return(T)
    } else {
      # print("DB does not exists")
      db <<- NULL
      return(F)
    }
  })
  
  outputOptions(output, "db_present", suspendWhenHidden=FALSE)

  # Main
  source(file.path("server", "server_main.R"),  local = TRUE)$value
  
  # Database
  source(file.path("server", "server_db.R"),  local = TRUE)$value

  # Plot
  source(file.path("server", "server_plot.R"),  local = TRUE)$value
})

# Run application 
shinyApp(ui = ui, server = server)
