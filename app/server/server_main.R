# QS trend analysis - server main 

output$main_text_1 <- renderUI({
  HTML(paste("Hi labbie,",
             "", 
             "Welcome to another R Shiny app! In this app you can plot the trending of your QS positive control samples.",
             "In the database tab you can import new QS files and add them to your database. Once the data is added to the database you can have a look at the trending by clicking on the 'plot' tab above.",
             "",
             "The database is a csv file that is stored in the directory of the QS_trend_analysis app and is called 'db.csv'. Also, a quality control file (qc.csv) can be added to the same directory to draw the QC standards for each target.",
             "",
             "",
             "Below you can see the directory where you either can find the db.csv file (after you have created it) and where you should put the qc.csv.",
             "<br/>",
             sep="<br/>"))
})

output$db_path <- renderUI({
  HTML(paste0("PATH: ", DB_PATH, "<br/>"))
})

output$main_text_2 <- renderUI({
  HTML(paste("",
             "When you already have some files in your database, you will see them if you move to the 'database' tab.",
             "<br/>",
             sep="<br/>"))
})
