# QS trend analysis - UI - sidebar

wellPanel(
  conditionalPanel(condition = "input.tabs == 'db'", 
                   fileInput(inputId = "input_files", 
                             label = "",
                             multiple = T,
                             accept = c(".txt"), 
                             placeholder = "Add new txt files")),
  
  # Works together with the observe button in app to reset fileInput widget
  tags$script('
    Shiny.addCustomMessageHandler("resetFileInputHandler", function(x) {      
        var id = "#" + x + "_progress";
        var idBar = id + " .bar";
        $(id).css("visibility", "hidden");
        $(idBar).css("width", "0%");
    });
  '), # TODO: still have to add: "Add new text files"
  
  # conditionalPanel(condition = "input.tabs == 'plot'",
  #                  sliderInput(inputId = "slider_years", label = "Years", value = c(2019, 2020), min = 2018, max = 2021),
  #                  sliderInput("slider_months", "Months", value = c(2, 12), min = 1, max = 12))
  conditionalPanel(condition = "input.tabs == 'plot'",
                   radioButtons(inputId = "radio_period", 
                                choices = c("Week", "Month", "Year"), 
                                label = "Select period", 
                                selected = "Month"))
  
)
