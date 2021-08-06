# QS trend analysis - server db

# Define reactive values ----
rv <- reactiveValues(
  db_avail = FALSE,
  db_data = NULL,
  new_data = NULL,
  clear = FALSE
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
                                     select(date, exp_name) %>% 
                                     mutate(date = as.character(date)) %>%
                                     arrange(date) %>%
                                     distinct(),
                                   options = list(pageLength = 10)
)

# Import new files ----
observe({
  req(input$input_files)
  req(!rv$clear)
  
  file_paths <<- input$input_files # TODO: replace <<- --> <-?
  
  # Preprocess files df
  # print(file_paths)
  file_paths <<- file_paths %>%
    rename(exp_name = name) %>% 
    mutate(exp_name = substr(exp_name, 0, nchar(exp_name)-4))
  
  # TODO: Validate files to prompt error message
  # mes <- validateFileDF(file_paths)
  
  # For now, import and preprocess data
  file_paths <<- file_paths %>% as_tibble() %>% 
    mutate(map_df(datapath, readTxt), 
           data = map(data, processQSResults))
  
  # Check if db is available in order to draw table
  if(is.null(rv$db_data)) {
    file_paths <<- file_paths %>% mutate(in_database = "No")
  } else {
    # Check if not available
    file_paths <<- file_paths %>% 
      mutate(in_database = if_else(exp_name %in% rv$db_data$exp_name, "Yes", "No"))
  }
    
  rv$new_data <- file_paths %>%
    select(date, exp_name, in_database) %>%
    mutate(date = as.character(date))
}
)

observeEvent(input$input_files, {
  rv$clear <- FALSE
})

# Check whether files are imported ----
output$files_imported <- reactive({
  return(!is.null(rv$new_data))
})

outputOptions(output, "files_imported", suspendWhenHidden=FALSE)

# Render new files ----
output$input_files_table <- renderDataTable(rv$new_data, options = list(pageLength = 10))

# Add files to database and reset ----
observeEvent(input$add_to_db, {
  # Check for presence database
  if(!is.null(rv$db_data)) {
    # Prepare x by binding the current db to the added samples
    if(sum(file_paths$in_database == "No") > 0) {
      db <- rv$db_data %>% 
        bind_rows(file_paths %>% 
                    filter(in_database == "No") %>% 
                    unnest(data) %>% 
                    select(date, exp_name, instrument, id, target, ct)) %>% 
                    arrange(date)  
    } else {
      db <- rv$db_data
    }
  }
  
  rv$db_data <- db
  
  # Prepare and save x
  write_csv(x = db, file = "./../db.csv")
  
  # Reset infiles and table
  rv$new_data <- NULL
  rv$clear <- TRUE
  rv$db_avail <- TRUE
  reset("input_files")
})
