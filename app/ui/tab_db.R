# QS trend analysis - tab db

tabPanel("Database", value = "db",
         conditionalPanel(condition = "!output.db_present",
                          h3("Please import files to create database")),
         
         conditionalPanel(condition = "output.db_present",
                          h3("Available experiments in database")),
         
         conditionalPanel(condition = "output.db_present",
                          dataTableOutput("db_files")),
         
         conditionalPanel(condition = "output.files_imported",
                          h3("Imported files")),
         
         conditionalPanel(condition = "output.files_imported",
                          dataTableOutput("input_files_table")),
         
         conditionalPanel(condition = "output.files_imported & output.no_errors",
                          actionButton("add_to_db", "")),
         
         conditionalPanel(condition = "output.files_imported & !output.no_errors",
                          h4("Errors found in input; at least one file is either already in the database or has an error."))
)
