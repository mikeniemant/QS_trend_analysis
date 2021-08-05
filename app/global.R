# QS trend analysis - global 
# Define global variables ----
# YES, you have made it to global.R file, nice job!
# You only have to edit the following two variables, Shiny does the rest!
# Make sure to keep the same format: text between "" and don't touch the parentheses!

# 1. Edit the database path ("C:/db.csv")
PATH <- "/Users/mn/Library/Mobile Documents/com~apple~CloudDocs/workspace/skyline_shiny_apps/QS_trend_analysis/"
# 2. Include your targets (if you have less then eleven targets, you can use the SkylineDx colours, happy dance)
TARGETS <- c("TARGET1", "TARGET2", "TARGET..")
TARGETS <- c("ACTB", "RPLP0", "MLANA", "ITGB3", "PLAT", "IL8", "GDF15", "LOXL4", "TGFBR1", "SERPINE2")

# Thank you! You can save and close this file now. Make sure to restart Shiny!

Sys.setenv(TZ="UTC")
DB_PATH <<- paste0(PATH, "testing/db.csv")
QS_FILE_PATH <- paste0(PATH, "testing/data/")

# Color scheme
rgb_colours <- c("29 79 110", 
                 "74 174 219",
                 "198 88 145",
                 "170 213 225",
                 "137 138 137",
                 "131 69 141",
                 "13 47 65",
                 "42 104 131",
                 "130 44 88",
                 "82 153 176")
colors <- sapply(strsplit(rgb_colours, " "), function(x)
  rgb(x[1], x[2], x[3], maxColorValue=255))

# Functions ----
readTxt <- function(file_path) {
  # Text file contains three parts
  # 1. run settings and info
  # 2. results
  # 3. melt curve results
  
  # First read file and check the number 
  con = file(file_path, "r")
  i = 1
  
  # Define objects
  exp_instr <- "Not available"
  exp_instr_id <- "Not available"
  
  while(TRUE) {
    line = readLines(con, n = 1)
    
    # Extract variables
    # - Experiment Run End Time
    # - Instrument Type
    # - Instrument Serial Number
    
    if(substr(line, 1, 28) == "* Experiment Run End Time = ") {
      exp_date <- substr(line, 29, nchar(line))
      #print(exp_date)
    }
    
    if(substr(line, 1, 26) == "* Instrument Name =       ") {
      exp_instr <- substr(line, 27, nchar(line))
      #print(exp_instr)
    }
    
    if(substr(line, 1, 29) == "* Instrument Serial Number = ") {
      exp_instr_id <- substr(line, 30, nchar(line))
      #print(exp_instr_id)
    }
    
    if(line == "[Results]") {
      break
    }
    i = i + 1
  }
  j = 0
  while(TRUE) {
    line = readLines(con, n = 1)
    if(line == "") {
      break
    }
    j = j + 1
  }
  close(con)
  
  # Read file
  res <- suppressMessages(readr::read_tsv(file_path, skip = i, n_max = j-1))
  
  # Fix date
  if(exp_date == "Not Started") {
    exp_date <- "Date error"
  } else if(substr(exp_date, nchar(exp_date)-2, nchar(exp_date)) == "CET" | substr(exp_date, nchar(exp_date)-3, nchar(exp_date)) == "CEST") {
    exp_date <- as.POSIXct(exp_date, format = "%d-%m-%Y %H:%M")
  } else if(substr(exp_date, nchar(exp_date)-2, nchar(exp_date)) == "PDT" | substr(exp_date, nchar(exp_date)-2, nchar(exp_date)) == "PST") {
    exp_date <- as.POSIXct(substr(exp_date, 1, 16), format = "%Y-%m-%d %H:%M")
  }
  # print(exp_date)
  
  # Compile output as list object
  tib <- tibble(date = exp_date,
                instr = as.double(exp_instr),
                id = as.double(exp_instr_id)) %>% 
    bind_cols(res %>% nest(data = everything()))
  
  return(tib)
}

processQSResults <- function(res, targets) {
  # Change Undetermined to NA
  res$CT[res$CT == "Undetermined"] <- NA
  # TODO: Can we improve this?
  
  # Filter relevant columns
  res <- res %>% 
    transmute(sample_id = `Sample Name`, 
              target = `Target Name`,
              ct = as.double(CT)) %>% 
    mutate(sample_id = case_when(sample_id == "REF" ~ "Positive Control",
                                 sample_id == "NTC" ~ "Negative Control",
                                 T ~ sample_id))
  
  # Filter relevant targets
  res <- res %>% 
    filter(target %in% targets)
  
  # Filter positive controls
  res <- res %>% filter(sample_id == "Positive Control")
  
  return(res)
}
