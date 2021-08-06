# QS trend analysis - global 
Sys.setenv(TZ="UTC")

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
                id = as.character(exp_instr_id)) %>% 
    bind_cols(res %>% nest(data = everything()))
  
  return(tib)
}

processQSResults <- function(res) {
  # Change Undetermined to NA
  res <- res %>% 
    mutate(CT = replace(CT, CT == "Undetermined", NA))
  
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

generatePlot <- function(db, qc, t, period) {
  x <- db %>% 
    filter(target == t) %>% 
    mutate(instrument = factor(instrument),
           d_days = as.integer(difftime(max(date), date, units = "days")))
  
  # Filter data based on selected period
  x <- x %>% 
    filter(d_days <= case_when(period == "Week" ~ 7,
                               period == "Month" ~ 31,
                               period == "Year" ~ 365,
                               period == "All" ~ as.double(max(x$d_days))))

  p <- ggplot(x) +
    geom_point(aes(x = date, 
                   y = ct, 
                   colour = target, 
                   shape = instrument,
                   text = paste0("Date: ", date, "\n",
                                 "Experiment name: ", x$exp_name, "\n",
                                 "Target: ", target, "\n",
                                 "Ct: ", ct, "\n",
                                 "Instrument: ", instrument, "\n")), alpha = 0.5) +
    labs(x = "Date",
         y = "Ct",
         colour = "Target name",
         shape = "Instrument",
         title = t) +
    scale_color_manual(values = colors[which(unique(db$target) %in% t)]) + 
    theme_bw() +
    theme(legend.position="none")
  
  if(!is.null(qc)) {
    # Preprocess dates for target 
    qc_t <- qc %>% 
      filter(target == t)
    
    # Create dynamic QC values
    # First, edit the minimal date
    if(min(qc_t$date) < min(x$date)) {
      # Remove all QC dates before, expect 1, before the min(x$date)
      if(all(which(qc_t$date < min(x$date)))) {
        qc_t <- qc_t %>% slice(nrow(.))
        qc_t$date[1] <- min(x$date)
      } else {
        most_recent_qc_date_i <- which(qc_t$date < min(x$date))-1
        qc_t$date[most_recent_qc_date_i]
        qc_t <- qc_t %>% slice(most_recent_qc_date_i:nrow(.))
        # qc_t$date[1] <- x$date[1]
      }
    }
    
    # Then, edit the maximal date
    if(max(qc_t$date) > max(x$date)) {
      qc_t$date[nrow(qc_t)] <- max(x$date)
    } else {
      # Add the latest
      qc_t <- qc_t %>% 
        bind_rows(tibble(target = t,
                         date = x %>% filter(target == t) %>% slice(which.max(date)) %>% pull(date),
                         min = qc %>% filter(target == t) %>% slice(which.max(date)) %>% pull(min),
                         max = qc %>% filter(target == t) %>% slice(which.max(date)) %>% pull(max)))
    }
    
    p <- p + 
      geom_step(aes(x = date, y = min), qc_t, colour = "black", linetype = "dashed") +
      geom_step(aes(x = date, y = max), qc_t, colour = "black", linetype = "dashed")
  }
  
  return(p)
}
