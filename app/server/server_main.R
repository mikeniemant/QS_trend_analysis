# QS trend analysis - server main 

output$main_text_1 <- renderUI({
  HTML(paste("Hi labbie,",
             "", 
             "Welcome to another R Shiny app! In this app you can plot the trending of your QS positive control samples.",
             "In the 'Database' tab you can import new QS files and add them to your database. Once the data is added to the database you can have a look at the trending by clicking on the 'Plot' tab above.",
             "",
             "The database is a csv file that is stored in the directory of the QS_trend_analysis app and is called 'db.csv'. Also, a quality control file (qc.csv) can be added to the same directory to draw the QC standards for each target.",
             "",
             "Below you can see the directory where you either can find the db.csv file (after you have created it) and where you should put the qc.csv (TRUE: shiny has found the file; FALSE: shiny could not find the file)",
             sep="<br/>"))
})

output$paths <- renderUI({
  HTML(paste0("- Database file (db.csv) in place: ", file.exists("./../db.csv"), "<br/>"),
       paste0("- Quality file (qc.csv) in place: ", file.exists("./../cv.csv"), "<br/>"),)
})
