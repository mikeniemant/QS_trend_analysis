# QS trend analysis - server main 

# Define main text
output$main_text_1 <- renderUI({
  HTML(paste("Hi labbie,",
             "", 
             "Welcome to another R Shiny app! In this app you can plot the trending of your QS positive control samples.",
             "On the left you can import new QS files and you can have a look at the trending by clicking on the 'plot' tab above.",
             "",
             "I can imagine that you would like to add new files to a sort of 'database' so you don't have to re-select all files from previous cohorts each time you want to look at the trending. Therefore, this app works with a database, which is basically a csv file stored on your machine that is updated each time you add some new files to it. After you have selected some new files, the software will first check whether the imported files can be added to the database. If so, a button will appear that you can click to add the imported files to the database.",
             "",
             "However, to make this work, I need you to do something for me. Edit the following two variables in the global.R file; the path where your database is located on your machine, if you don't have one Shiny will automatically create one for you the first time you import some files. Second, the targets you are interested in.",
             "",
             "You can edit both variables in the global.R file which you can find in the directory of this shiny app:",
             '- Database path (PATH): example; "PATH <- C:/db.csv"',
             '- Targets (TARGETS): example; TARGETS <- c("TARGET1", "TARGET2", "TARGET..")',
             "",
             "After you have edited the two variables, make sure to restart Shiny!",
             "",
             "Below you can see these two variables to check whether you have edited them correctly.",
             "<br/>",
             sep="<br/>"))
})

# Define db path
output$db_path <- renderUI({
  HTML(paste0("PATH: ", DB_PATH, "<br/>"))
})

output$main_text_2 <- renderUI({
  HTML(paste("",
             "When you already have some files in your database, you will see them if you move to the 'database' tab.",
             "<br/>",
             sep="<br/>"))
})

# Define targets
output$targets <- renderUI({
  HTML(paste0("TARGETS: ", paste0(TARGETS, collapse = ", ")))
})
