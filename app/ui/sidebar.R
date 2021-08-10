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
  #                  sliderInput(inputId = "slider_years", label = "Years", value = c(2019, 2020), min = 2018, max = 2021),
  #                  sliderInput("slider_months", "Months", value = c(2, 12), min = 1, max = 12))
  conditionalPanel(condition = "input.tabs == 'plot'",
                   radioButtons(inputId = "radio_period", 
                                choices = c("Last week", "Last month", "Last year", "All"), 
                                label = "Select period", 
                                selected = "Last month"))
)
