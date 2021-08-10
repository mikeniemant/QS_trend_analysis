# QS trend analysis - server plot

# Date calendar ----
observe({
  req(rv$db_avail)
  output$dateRange <- renderUI({
    dateRangeInput("date", "Select date range:",
                   start = 
                     as.character(format(as.Date(min(rv$db_data$date))),"yyyy-mm-dd"),
                   end = 
                     as.character(format(as.Date(max(rv$db_data$date))),"yyyy-mm-dd"),
                   min = 
                     as.character(format(as.Date(min(rv$db_data$date))),"yyyy-mm-dd"),
                   max = 
                     as.character(format(as.Date(max(rv$db_data$date))),"yyyy-mm-dd"),
                   format = "yyyy-mm-dd")
  })
})

# Plot ----
observeEvent({
  
  c(input$tabs, input$date, input$input_files) # input$radio_period, 
},
{
  req(rv$db_avail)
  targets <<- unique(rv$db_data$target)

  # Filter data with calendar
  if(!is.null(input$date)) {
    x <- rv$db_data %>% filter(date >= input$date[1] & date <= input$date[2])
    qc_trim <- qc %>% filter(date >= input$date[1] & date <= input$date[2])
  } else{
    return(NULL)
  }
  
  # Filter data with radio button
  # x <- x %>% 
  #   filter(d_days <= case_when(period == "Last week" ~ 7,
  #                              period == "Last month" ~ 31,
  #                              period == "Last year" ~ 365,
  #                              period == "All" ~ as.double(max(x$d_days))))
  
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

      output[[plotname]] <- renderPlotly({
        suppressWarnings(ggplotly(generatePlot(x, qc_trim, target), tooltip = "text"))
        # TODO: remove suppressWarnings, however there are still two errors in this functions
        # 1. the generated text is defined in the geom_point function instead of the standard ggplot function
        # Otherwise, the geom_step function will always take over these setting leading to an error (because there is not ct in qc)
        # 2. The dynamic qc data frame is still in production
      })
    })
  }
})
