# QS trend analysis - server plot

observeEvent({
  
  c(input$tabs, input$radio_period)
},
{
  if(is.null(db)) {
    return(NULL)
  } else {
    # Compute number of days
    x <- db %>% mutate(d_days = as.integer(difftime(max(date), date, units = "days")))
    if(input$radio_period == "Week") {
      x <- x %>% filter(d_days <= 7)
    } else if(input$radio_period == "Month") {
      x <- x %>% filter(d_days <= 31)
    } else {
      x <- x %>% filter(d_days <= 365)
    }
    
    p <- plotTrend(x)

    output$plot_trend <- renderPlotly(ggplotly(p, height=1600))
  }
})
