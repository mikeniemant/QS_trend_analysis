# QS trend analysis - server db

# Define reactive values ----
rv <- reactiveValues(
  db_avail = FALSE,
  db_data = NULL,
  val_import = NULL,
  import_dat = NULL,
  clear = TRUE # FALSE
)

# Check presence and import database ----
if(file.exists("./../db.csv")) {
  rv$db_data <- read_csv("./../db.csv",
                         col_types = list(col_datetime(format = ""), col_character(),
                                          col_character(), col_character(),
                                          col_character(), col_double())) %>%
    arrange(date)
  rv$db_avail <-  TRUE
}

output$db_present <- reactive({
  return(rv$db_avail)
})

outputOptions(output, "db_present", suspendWhenHidden=FALSE)

# Render database ----
output$db_files <- renderDataTable(rv$db_data %>% 
                                     select(date, exp_name, instrument, id) %>% 
                                     mutate(date = as.character(date)) %>%
                                     arrange(date) %>%
                                     distinct(),
                                   options = list(pageLength = 10)
)

# Import new files ----
observe({
  req(input$input_files)
  req(!rv$clear)
  
  file_paths <- input$input_files
  
  file_paths <- validateFiles(file_paths)
  
  # Check if db is available in order to draw table
  if(!is.null(rv$db_data)) {
    # Check if not available
    file_paths <- file_paths %>% 
      mutate(error = replace(error, exp_name %in% rv$db_data$exp_name, "Already in database"))
  }
  
  rv$import_dat <- file_paths %>% 
    unnest(data) %>% 
    select(date, exp_name, id, instrument, target, ct)
  
  rv$val_import <- file_paths %>% 
    select(date, exp_name, error) %>%
    mutate(date = as.character(date))
  
  # Update button
  updateActionButton(session, "add_to_db",
                     label = paste("Import ", sum(file_paths$error  == ""), " files"))
}
)

observeEvent(input$input_files, {
  rv$clear <- FALSE
})

# Check whether files are imported ----
output$files_imported <- reactive({
  return(!is.null(rv$val_import))
})

outputOptions(output, "files_imported", suspendWhenHidden=FALSE)

# Render new files ----
output$input_files_table <- renderDataTable(rv$val_import, 
                                            options = list(pageLength = 10))

# Check whether there are no errors ----
output$no_errors <- reactive({
  return(all(rv$val_import$error == "" | rv$val_import$error == "Already in database"))
})

outputOptions(output, "no_errors", suspendWhenHidden=FALSE)

# Add files to database and reset ----
observeEvent(input$add_to_db, {
  # Check for presence database
  if(!is.null(rv$db_data)) {
    db <- rv$db_data %>%
      bind_rows(rv$import_dat)
  } else {
    db <- rv$import_dat
  }
  
  rv$db_data <- db %>% 
    arrange(date)

  # Prepare and save x
  write_csv(x = db, file = "./../db.csv")

  # Reset infiles and table
  rv$val_import <- NULL
  rv$clear <- TRUE
  rv$db_avail <- TRUE
  reset("input_files")
})
