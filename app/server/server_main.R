# QS trend analysis - server main 

output$main_text <- renderUI({
  HTML(paste("Hi labbie,",
             "", 
             "Welcome to another R Shiny app! In this app you can plot the 
             trending of your QS positive control samples.",
             "In the 'Database' tab you can import new QS files and add them to 
             your database. Once the data is added to the database you can have 
             a look at the trending by clicking on the 'Plot' tab above.",
             "",
             "The database is a csv file that is stored in the directory of the
             QS_trend_analysis app and is called 'db.csv'. Also, a quality 
             control file (qc.csv) can be added to the same directory to draw 
             the QC Ct limits for each target.",
             "",
             "Below you can see whether the shiny app has found both files
             (TRUE: shiny has found the file; FALSE: shiny 
             could not find the file):<br/>",
             sep="<br/>"))
})

output$paths <- renderUI({
  HTML(paste0("- Database file (db.csv) in place: ", file.exists("./../db.csv"), "<br/>"),
       paste0("- Quality file (qc.csv) in place: ", file.exists("./../qc.csv"), "<br/>"),)
})

output$qc_text <- renderUI({
  HTML("<br/>It looks like did not have specified the qc.csv file yet, you can do this by creating a csv file in the directory of the app. The output should look like the table below:<br/>")
})

output$qc_example_table <- renderDataTable(data.frame(target = rep(paste0("TARGET_", 1:3), each = 2), 
                                                      date = rep(c("2020-01-01 00:00:00", "2020-01-02 00:00:00", "2020-01-03 00:00:00"), 2),
                                                      min = round(rnorm(n = 6, mean = 22, sd = 1), 2),
                                                      max = round(rnorm(n = 6, mean = 28, sd = 1), 2)),
                                            options = list(dom = 't'))
