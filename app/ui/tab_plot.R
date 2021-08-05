# TCU trend analysis - tab plot

tabPanel("Plot", value = "plot",
        
         plotlyOutput("plot_trend")
         
         # conditionalPanel(condition = "output.file_uploaded", #  & input.tabs == 'plot'
         #                  plotOutput("plot"))
         
)
