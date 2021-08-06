# QS trend analysis - server db

# Import new files ----
output$files_imported <- reactive({
  # Extract imputed files
  file_paths <<- input$input_files
  
  # If file_paths is not NULL (global setting), "if files are selected.."
  if(!is.null(file_paths)) {
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
    if(is.null(db)) {
      file_paths <<- file_paths %>% mutate(in_database = "No")
      
    } else {
      # Check if not available
      file_paths <<- file_paths %>% 
        mutate(in_database = if_else(exp_name %in% db$exp_name, "Yes", "No"))
    }
    
    # Render files df
    output$input_files_table <- renderDataTable(file_paths %>% 
                                                  select(date, exp_name, in_database), 
                                                options = list(dom = 't'))
    
    # print(sum(file_paths$in_database == "No"))
    return(sum(file_paths$in_database == "No") > 0)
  } else {
    return(F)
  }
})

outputOptions(output, "files_imported", suspendWhenHidden=FALSE)

# Add files to database ----
observeEvent(input$add_to_db, {
  # Add files to database
  x <- file_paths %>% 
    filter(in_database == "No") %>% 
    unnest(data) %>% 
    select(date, exp_name, instrument, id, target, ct)
  
  if(is.null(db)) {
    db <<- x
  } else {
    # Prepare x by binding the current db to the added samples
    db <<- db %>% bind_rows(x)
  }
  
  # Prepare and save x
  write_csv(x = db, file = DB_PATH)
  
  # Reset 
  # file_paths <<- NULL
  renderDataTable(NULL,
                  options = list(dom = 't'))
  # TODO: reset new file tables
})

# Reset the input_file button
observe({
  input$add_to_db
  session$sendCustomMessage(type = "resetFileInputHandler", "input_files")  
})

# observeEvent({
#   
#   c(input$tabs, input$radio_mm, input$input_files)
# },
# {
#   
# }
# )
