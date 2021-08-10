# QS trend analysis - ui sidebar

tabPanel("Introduction", value = "main",
         htmlOutput("main_text"),
         
         htmlOutput("paths"), 
         
         conditionalPanel(condition = "!output.qc_present",
                          htmlOutput("qc_text"),
                          dataTableOutput("qc_example_table"))
)
