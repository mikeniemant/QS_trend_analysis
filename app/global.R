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
      return(if_else(length(n_lines)< 1, "Empty file", ""))
    }))
  
  # Extract meta data and check if any of meta data is NA
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
  
  
  # Extract ct data
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
  # print(exp_date)
  
  # Compile output as list object
  tib <- tibble(date = exp_date,
                instrument = as.character(exp_instr),
                id = as.character(exp_instr_id))
  
  return(tib)
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
  
  res <- read_tsv(file = file_path, 
                  skip = i+1, 
                  n_max = j-1, 
                  col_names = c("Well", "Sample Name", "Target Name", "CT"),
                  col_types = c(col_character(), col_character(), col_character(), col_character()))
  
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

generatePlot <- function(db, qc = NULL, t, period) {
  x <- db %>% 
    filter(target == t) %>% 
    mutate(instrument = factor(instrument),
           d_days = as.integer(difftime(max(date), date, units = "days")))
  
  # Filter data based on selected period
  x <- x %>% 
    filter(d_days <= case_when(period == "Last week" ~ 7,
                               period == "Last month" ~ 31,
                               period == "Last year" ~ 365,
                               period == "All" ~ as.double(max(x$d_days))))

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

simulateQSFile <- function(n = 10, 
                           start_date = "2001-01-01 12:00:00",
                           end_date = "2001-01-10 12:00:00",
                           sim_date = F,
                           target_dic = tibble(target = c("ACTB", "RPLP0", "MLANA", "ITGB3", "PLAT", "IL8", "GDF15", "LOXL4", "TFGBR1", "SERPINE2"),
                                               mean = c(20, 21, 25, 28, 27, 25, 25, 31, 25, 22)),
                           sd = 0.1,
                           dir) {
  # Simulate QS files with speficic dates and CT output for a prespecified 
  # number of targets with corresponding
  # n: number of required samples
  # start_date: start date
  # end_date: end data to simulate experiment dates
  # target_dic: targets with corresponding mean ct values
  # dir: output directory
  #
  # Example1: simulateQSFile(n = 15, dir = "./../testing/test/")
  # Example2: simulateQSFile(n = 15, start_date = "2010-01-01 12:00:00", end_date = "2020-01-01 00:00:00", dir = "./../testing/test/")
  
  # Generate expirement names and dates
  exp_names <- paste0("exp_", 1:n)
  
  start_date = as.POSIXct(start_date)
  end_date = as.POSIXct(end_date)
  
  if(as.integer(difftime(end_date, start_date, units = "days")) < n) {
    cat("Start date: ", start_date, 
          "\nEnd date: ", end_date,
          "\nn: ", n,
          "\n\nAs we do not want to have duplicate date runs and the function simulates dates wiht a difference of one day. Make sure that the 'n' input variable is greater than the difference between 'end_date' and 'start_date in days.")
    return()
  }
  
  if(sim_date) {
    exp_dates <- seq(from = start_date, to = end_date, by = "D")
  } else {
    exp_dates <- sample(seq(start_date, end_date, by="day"), n, replace = F)
  }
  
  exp_dates <- paste0(format(exp_dates, "%d-%m-%Y %H:%M:%S"), " CEST")  
  
  # Generate data
  for(i in 1:n) {
    file <- paste0(dir, exp_names[i], ".txt")
    if(file.exists(file)) file.remove(file)
    sink(file, append = T)
    
    # Table 1
    t1 <- paste("* Block Type = 96-Well 0.2-mL Block", 
                "* Calibration Background is expired  = No",
                "* Calibration Background performed on = 01-01-2001",
                "* Calibration Pure Dye ABY is expired = No",
                "* Calibration Pure Dye ABY performed on = 01-01-2001",
                "* Calibration Pure Dye CY5 is expired = No",
                "* Calibration Pure Dye CY5 performed on = 01-01-2001",
                "* Calibration Pure Dye FAM is expired = No",
                "* Calibration Pure Dye FAM performed on = 01-01-2001",
                "* Calibration Pure Dye JUN is expired = No",
                "* Calibration Pure Dye JUN performed on = 01-01-2001",
                "* Calibration Pure Dye MUSTANG PURPLE is expired = No",
                "* Calibration Pure Dye MUSTANG PURPLE performed on = 01-01-2001",
                "* Calibration Pure Dye NED is expired = No",
                "* Calibration Pure Dye NED performed on = 01-01-2001",
                "* Calibration Pure Dye ROX is expired = No",
                "* Calibration Pure Dye ROX performed on = 01-01-2001",
                "* Calibration Pure Dye SYBR is expired = No",
                "* Calibration Pure Dye SYBR performed on = 01-01-2001",
                "* Calibration Pure Dye TAMRA is expired = No",
                "* Calibration Pure Dye TAMRA performed on = 01-01-2001",
                "* Calibration Pure Dye VIC is expired = No",
                "* Calibration Pure Dye VIC performed on = 01-01-2001",
                "* Calibration ROI is expired  = No",
                "* Calibration ROI performed on = 01-01-2001",
                "* Calibration Uniformity is expired  = No",
                "* Calibration Uniformity performed on = 01-01-2001",
                "* Chemistry = SYBR_GREEN",
                "* Date Created = 01-01-2001 00:00:00 CEST",
                "* Experiment Barcode = NA",
                "* Experiment Comment = NA",
                "* Experiment File Name = UNKNOWN",
                paste0("* Experiment Name = ", exp_names[i]),  # 20180725_PROT-127_Plate1_BridgingSampleQC
                paste0("* Experiment Run End Time = ", exp_dates[i]), # 23-08-2018 15:37:27 CEST
                "* Experiment Type = Standard Curve",
                "* Instrument Name =       QS-001",
                "* Instrument Serial Number = 001",
                "* Instrument Type = QuantStudio™ 5 Dx System",
                "* Passive Reference = ROX",
                "* Post-read Stage/Step = ",
                "* Pre-read Stage/Step = ",
                "* Quantification Cycle Method = Ct",
                "* Signal Smoothing On = true",
                "* Stage where Melt Analysis is performed = Stage3",
                "* Stage/ Cycle where Ct Analysis is performed = Stage2, Step2",
                "* User Name = NA",
                "",
                sep = "\n")
    writeLines(t1)
    
    # Table 2
    writeLines("[Results]")
    
    # Count number of required samples
    n_sim <- floor((96 - (nrow(target_dic))) / nrow(target_dic))
    
    # Simulate data 
    t2 <- target_dic %>% 
      mutate(ct = map(mean, function(x) rnorm(n = n_sim, mean = x, sd = sd))) %>% 
      unnest(ct) %>% 
      mutate(ct = as.character(round(ct, 3))) %>% 
      select(-mean)
    
    # Add sample names
    t2 <- t2 %>% 
      mutate(sample_name = rep(paste0(exp_names[i], "_", 1:n_sim), times = nrow(target_dic)))
    
    # Create Positive Control
    t2 <- t2 %>% 
      mutate(sample_name = replace(sample_name, 
                                   sample_name == paste0(exp_names[i], "_", n_sim), 
                                   "Positive Control"))
    
    # Add Negative Control
    t2 <- t2 %>% bind_rows(tibble(target = target_dic$target,
                                  ct = "Undetermined",
                                  sample_name = "Negative Control"))
    
    # Add well and finalize output
    t2 <- t2 %>% 
      arrange(match(sample_name, c("Positive Control", "Negative Control", paste0(exp_names[i], "_", 1:(n_sim-1))))) %>% 
      mutate(Well = row_number()) %>% 
      transmute(Well = row_number(),
                `Sample Name` = sample_name,
                `Target Name` = target,
                CT = ct)
    
    writeLines(paste(colnames(t2), collapse = "\t"))
    
    for(j in 1:nrow(t2)) {
      writeLines(paste(unname(as.matrix(t2)[j, ]), collapse = "\t"))
    }
    
    # t3
    writeLines("\n[Melt Curve Results]")
    t3 <- t2 %>% 
      select(-CT) %>% 
      mutate(Tm = 0)
    
    writeLines(paste(colnames(t3), collapse = "\t"))
    
    for(j in 1:nrow(t3)) {
      writeLines(paste(unname(as.matrix(t3)[j, ]), collapse = "\t"))
    }
    sink()
    closeAllConnections()
  }
}
