# QS trend analysis - tab db

tabPanel("Database", value = "db",
         
         conditionalPanel(condition = "!output.db_present",
                          h3("Please import files to initiate database")),
         
         conditionalPanel(condition = "output.db_present",
                          h3("Available experiments in database")),
         
         conditionalPanel(condition = "output.db_present",
                          dataTableOutput("db_files")),
         
         conditionalPanel(condition = "output.files_imported",
                          h3("Imported files")),
         
         dataTableOutput("input_files_table"),
         
         conditionalPanel(condition = "output.files_imported",
                          actionButton("add_to_db", "Add data to database"))
)
