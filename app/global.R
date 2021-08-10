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
validateFiles <- function(file_paths) {
  # Preprocess files df
  file_paths <- file_paths %>%
    rename(exp_name = name) %>% 
    mutate(exp_name = substr(exp_name, 0, nchar(exp_name)-4)) %>% 
    as_tibble()
  
  # Check if file is empty
  file_paths <- file_paths %>% 
    mutate(error = map_chr(datapath, function(x) {
      # VS1: check if file is empty meta data
      con = file(x, "r")
      n_lines <- readLines(con)
      close(con)
      return(if_else(length(n_lines) < 1, "Empty file", ""))
    }))
  
  # VS2: Check if file has the meta data
  file_paths <- file_paths %>% 
    mutate(map_df(datapath, readMeta)) %>% 
    mutate(error = if_else(sum(is.na(c(date, instrument, id))) == 3, "No meta", "")) %>% 
    select(-c(date, instrument, id))
  
  # Extract meta data depending on available files
  # - from multiple files with at least one error and one without error
  # - from multiple files with no error and one without error
  # - from one file with no errors
  if(sum(file_paths$error != "") > 0 & sum(file_paths$error == "") > 0) {
    file_paths <- bind_rows(file_paths %>% 
                              filter(error != "") %>% 
                              mutate(date = as.POSIXct(NA),
                                     instrument = as.character(NA),
                                     id = as.character(NA)),
                            file_paths %>% 
                              filter(error == "") %>% 
                              mutate(map_df(datapath, readMeta)) %>% 
                              rowwise() %>% 
                              mutate(error = if_else(sum(is.na(c(date, instrument, id))) > 0, 
                                                     "Meta error", 
                                                     "")) %>% 
                              ungroup())
    
    file_paths <- bind_rows(file_paths %>% 
                              filter(error != "") %>% 
                              mutate(data = list(NULL)),
                            file_paths %>% 
                              filter(error == "") %>% 
                              mutate(data = map(datapath, processQSResults)))
    
  } else if(sum(file_paths$error != "") > 0 & sum(file_paths$error == "") == 0) {
    file_paths <- file_paths %>% 
      filter(error != "") %>% 
      mutate(date = as.POSIXct(NA),
             instrument = as.character(NA),
             id = as.character(NA),
             data = list(NULL))
  } else {
    file_paths <- file_paths %>% 
      filter(error == "") %>% 
      mutate(map_df(datapath, readMeta)) %>% 
      rowwise() %>% 
      mutate(error = if_else(sum(is.na(c(date, instrument, id))) > 0, 
                             "Meta error", 
                             "")) %>% 
      ungroup()
    
    file_paths <- file_paths %>% 
      filter(error == "") %>% 
      mutate(data = map(datapath, processQSResults))
  }

  return(file_paths)
}

readMeta <- function(file_path) {
  # First read file and check the number 
  con = file(file_path, "r")
  i = 1
  
  # Define objects
  exp_date <- NA
  exp_instr <- as.character(NA)
  exp_instr_id <- as.character(NA)
  
  while(TRUE) {
    line = readLines(con, n = 1)
    
    if(length(line) == 0) break
    
    # Extract meta data
    # - Experiment Run End Time
    # - Instrument Type
    # - Instrument Serial Number
    if(substr(line, 1, 28) == "* Experiment Run End Time = ") {
      exp_date <- substr(line, 29, nchar(line))
      #print(exp_date)
    }
    
    if(substr(line, 1, 26) == "* Instrument Name =       ") {
      exp_instr <- substr(line, 27, nchar(line))
      # print(paste0("Instrument Name: ", exp_instr))
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
  close(con)
  
  # Fix date
  if(exp_date != "Not Started" & !is.na(exp_date)) {
    if(substr(exp_date, nchar(exp_date)-2, nchar(exp_date)) == "CET" | substr(exp_date, nchar(exp_date)-3, nchar(exp_date)) == "CEST") {
      exp_date <- as.POSIXct(exp_date, format = "%d-%m-%Y %H:%M")
    } else if(substr(exp_date, nchar(exp_date)-2, nchar(exp_date)) == "PDT" | substr(exp_date, nchar(exp_date)-2, nchar(exp_date)) == "PST") {
      exp_date <- as.POSIXct(substr(exp_date, 1, 16), format = "%Y-%m-%d %H:%M")
    }
  }
  
  output <- tibble(date = exp_date,
                instrument = as.character(exp_instr),
                id = as.character(exp_instr_id))
  
  return(output)
}

processQSResults <- function(file_path) {
  con = file(file_path, "r")
  i = 1
  
  while(TRUE) {
    line = readLines(con, n = 1)
    if(line == "[Results]") {
      break
    }
    i = i + 1
  }
  
  # Read file
  j = 0
  while(TRUE) {
    line = readLines(con, n = 1)
    if(line == "") {
      break
    }
    j = j + 1
  }
  close(con)
  
  res <- suppressMessages(read_tsv(file = file_path, 
                  skip = i, 
                  n_max = j-1) %>% 
    select(Well, `Sample Name`, `Target Name`, CT))
  
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

generatePlot <- function(db, qc = NULL, t) {
  x <- db %>% 
    filter(target == t) %>% 
    mutate(instrument = factor(instrument),
           d_days = as.integer(difftime(max(date), date, units = "days")))
  
  p <- ggplot(x) +
    geom_smooth(aes(x = date, y = ct), 
                data = x, 
                method = lm, 
                formula = 'y ~ x', 
                se = FALSE, colour = colors[which(unique(db$target) %in% t)]) + 
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
      filter(target == t) %>% 
      arrange(date)
    
    # Create dynamic QC values
    # First, edit the minimal date
    if(min(qc_t$date) < min(x$date)) {
      # Remove all QC dates before, expect 1, before the min(x$date)
      if(all(qc_t$date < min(x$date))) {
        qc_t <- qc_t %>% slice(nrow(.))
        qc_t$date[1] <- min(x$date)
      }
    }
    
    # Now, edit the maximal date
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
