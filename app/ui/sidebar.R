# QS trend analysis - ui sidebar

wellPanel(
  useShinyjs(), # do not remove this, required to reset the fileInput object
  conditionalPanel(condition = "input.tabs == 'db'", 
                   fileInput(inputId = "input_files", 
                             label = "",
                             multiple = T,
                             accept = c(".txt"), 
                             placeholder = "Add new txt files")),
  
  # conditionalPanel(condition = "input.tabs == 'plot'",
  #                  radioButtons(inputId = "radio_period", 
  #                               choices = c("Last week", "Last month", "Last year", "All"), 
  #                               label = "Select period", 
  #                               selected = "Last month")),
  
  conditionalPanel(condition = "input.tabs == 'plot'",
                   uiOutput("dateRange"))
)
