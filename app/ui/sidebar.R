# QS trend analysis - UI - sidebar

wellPanel(
  conditionalPanel(condition = "input.tabs == 'db'", 
                   fileInput(inputId = "input_files", 
                             label = "",
                             multiple = T,
                             accept = c(".txt"), 
                             placeholder = "Add new txt files")),
  
  # conditionalPanel(condition = "input.tabs == 'plot'",
  #                  sliderTextInput(inputId = "slider_years", 
  #                                  label = "Years", 
  #                                  choices = c("2016", "2017", "2018", "2021"), 
  #                                  selected = min = 1, max = 3),
  #                  sliderInput("slider_months", "Months", value = c(3, 5), min = 1, max = 12)),
  
  # Works together with the observe button in app
  tags$script('
    Shiny.addCustomMessageHandler("resetFileInputHandler", function(x) {      
        var id = "#" + x + "_progress";
        var idBar = id + " .bar";
        $(id).css("visibility", "hidden");
        $(idBar).css("width", "0%");
    });
  ') # TODO: still have to add: "Add new text files"
)
