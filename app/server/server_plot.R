# QS trend analysis - server plot

observeEvent({
  
  c(input$tabs, input$radio_period)
},
{
  targets <<- unique(rv$db_data$target)
  
  # Insert the right number of plot output objects into the web page
  output$plots <- renderUI({
    plot_output_list <- lapply(1:length(targets), function(i) {
      plotname <- paste("plot", targets[i], sep="")
      plotlyOutput(plotname) #, height = 280, width = 800)
    })
    
    # Convert the list to a tagList - this is necessary for the list of items
    # to display properly.
    do.call(tagList, plot_output_list)
  })
  
  # Generate all plots and save in local output
  for(i in targets) {
    local({
      target <- i # apparently, we have to make this variable otherwise the global parameter 'i' is not moved to local
      plotname <- paste("plot", target, sep="")
      # print(plotname)
      output[[plotname]] <- renderPlotly({
        suppressWarnings(ggplotly(generatePlot(rv$db_data, qc, target, input$radio_period), tooltip = "text"))
        # TODO: remove suppressWarnings, however there are still two erros in this functions
        # 1. the generated text is defined in the geom_point function instead of the standard ggplot function
        # Otherwise, the geom_step function will always take over these setting leading to an error (because there is not ct in qc)
        # 2. The dynamic qc data frame is still in production
      })
    })
  }
})
