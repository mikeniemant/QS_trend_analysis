# QS trend analysis - global 
Sys.setenv(TZ="UTC")

DB_PATH <- "./../db.csv"
QC_PATH <- "./../qc.csv"

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
      # print(paste0("Intstrument Name: ", exp_instr))
    }
    
    if(substr(line, 1, 29) == "* Instrument Serial Number = ") {
      exp_instr_id <- substr(line, 30, nchar(line))
      # print(paste0("Instrument Serial Number: ", exp_instr_id))
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
                instrument = as.character(exp_instr),
                id = as.double(exp_instr_id)) %>% 
    bind_cols(res %>% nest(data = everything()))
  
  return(tib)
}

processQSResults <- function(res) {
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
  
  # Filter positive controls
  res <- res %>% filter(sample_id == "Positive Control")
  
  return(res)
}

plotTrend <- function(x) {
  p <- ggplot(x %>% mutate(instrument = factor(instrument)),
              aes(x = date, y = ct, colour = target)) +
    geom_point(aes(shape = instrument), alpha = 0.5) +
    stat_smooth(method = "lm", formula = "y ~ x") + 
    labs(x = "Date",
         y = "Ct",
         colour = "Target name",
         shape = "Instrument") +
    scale_color_manual(values = colors) +
    facet_wrap(~target, ncol = 1, scales = "fixed") + # free_y
    theme_bw() +
    theme(legend.position="none")
}
