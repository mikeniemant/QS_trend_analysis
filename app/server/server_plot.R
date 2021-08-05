# QS trend analysis - server plot

observeEvent({
  
  c(input$tabs) # , input$input_files
},
{
  if(is.null(db)) {
    return(NULL)
  } else {
    
    p <- ggplot(db %>% mutate(instr = factor(instr)),
                aes(x = date, y = ct, colour = target)) +
      geom_point(aes(shape = instr), alpha = 0.5) + #
      geom_line() +
      labs(x = "Date",
           colour = "Target name",
           shape = "Instrument") +
      scale_color_manual(values=colors) +
      facet_wrap(~target, ncol = 5, scales = "fixed") +
      theme_bw() +
      theme(legend.position="none")
    
    output$plot_trend <- renderPlotly({
      ggplotly(p)
    })
  }
})
