tryCatch(
  {
    # cleanup the environment ----
    rm(list = ls())
    if (!is.null(dev.list())) dev.off()
    cat("\014")
    start_time <- Sys.time()
    
    # capture variable coming from vba ----
    args <- commandArgs(trailingOnly=T)
    
    # set working director ---- 
    setwd(do.call(file.path, as.list(strsplit(args[1], "\\|")[[1]])))
    
    # load environment ----
    load("env.RData")
    load("dt_01_D.Rda")
    
    # load librarise ----
    error = f_libraries(
      necessary.std = c("glue", "dplyr", "rlang", "stringr"),
      necessary.github = c()
    )
    glue::glue("RUNNING R SERVER ...") %>% print()
    glue::glue("Package status: {error}") %>% print()
    glue::glue("\n") %>% print()

    # Code specific inputs ----
    purpose <- "Ensuring that numeric columns are appropriately recorded after removing NA values and outliers, and generating report"
    
    code_full <- scriptName::current_filename()
    code_path <- ifelse(is.null(code_full), "", dirname(code_full)) 
    code_name <- ifelse(is.null(code_full), "", basename(code_full))
    
    # Log of run ----
    glue::glue("===================== Running '{code_name}' =====================") %>% f_log_string(g_file_log) 
    glue::glue("{purpose}")%>% f_log_string(g_file_log)
    glue::glue("\n") %>% f_log_string(g_file_log)
    
    #====================================================
    
    print(glue::glue("Picking inputs on numeric columns from the excel interface..."))
    map <- f_read_xl(g_file_path, namedRegion = "body_numeric", colNames = F) %>% 
      unique()%>% 
      filter_all(any_vars(!is.na(.)))
    
    dt_01_E <- dt_01_D
    
    print(glue::glue("Ensuring all numeric columns are logged correctly..."))
    
    numb <- map %>% 
      filter(X2 == "Yes")
    var_numeric <- numb[["X1"]]
    
    if (length(var_numeric) > 0){
      for (var in var_numeric){
        
        var_sym <- var %>% rlang::sym()
        
        tryCatch(
          {
            # try part
            dt_01_E[,var] = as.numeric(dt_01_E[,var])
          },
          warning = function(w){
            # catch part
            print(glue::glue("!! String values in numeric column '{var}' that will generate 'NA':"))
            dt_01_E %>% 
              select(all_of(var)) %>%
              table() %>% 
              as.data.frame() %>% 
              select(Value = 1, Freq) %>% 
              filter(stringr::str_detect(Value, "^[+-]?(\\d*\\.?\\d+|\\d+\\.?\\d*)$", negate = T)) %>%
              filter(stringr::str_detect(Value, "^\\s*$", negate = T)) %>%
              print()
            print(glue::glue("-------------------------"))
            return(NULL)
          },
          finally={
            # do part - replace all such string response with octa-9 and blanks with NA
            dt_01_E <- dt_01_E %>% 
              mutate(!!var_sym := ifelse(stringr::str_detect(!!var_sym, "^[+-]?(\\d*\\.?\\d+|\\d+\\.?\\d*)$", negate = T) &
                                           stringr::str_detect(!!var_sym, "^\\s*$", negate = T), "99999999", !!var_sym))
            
            dt_01_E[,var] <- as.numeric(dt_01_E[,var]) %>%
              suppressWarnings()
          }
        )
      }
    }
    
    print(glue::glue("Ensuring all string columns with one or more numeric response are logged correctly..."))
    
    char <- map %>% 
      filter(X2 == "No")
    var_char <- char[["X1"]]
    
    if (length(var_char) > 0){
      for (var in var_char){
        dt_01_E[,var] = as.character(dt_01_E[,var])
      }
    }
    
    print(glue::glue("Identifying NA responses..."))        
    map_na <- map %>% 
      filter(X5 != "~") %>% 
      mutate(na_values = paste0("c(", str_replace_all(trimws(X5), " \\| ", ", "), ")")) %>% 
      select(X1, na_values)
    
    var_na <- map_na[["X1"]]
    
    if (length(var_na) > 0){
      for (var in var_na){
        
        var_sym <- var %>% 
          rlang::sym()
        
        condn <- map_na %>% 
          filter(X1 == var) %>% 
          pull(na_values)
        
        condn <- paste0(var, " %in% ", condn)
        
        dt_01_E <- dt_01_E %>% 
          mutate(!!var_sym := ifelse(eval(parse(text=condn)), 99999999, !!var_sym))
      }
    }
    
    print(glue::glue("Replacing outlier responses with NA entries..."))
    
    map_outlier <- map %>% 
      filter(X2 == "Yes") %>%
      mutate(X3 = ifelse(X3 == "~", -1000000000000000, X3),
             X4 = ifelse(X4 == "~", 1000000000000000, X4))
    
    var_outlier <- map_outlier[["X1"]]
    
    dt_01_Octa9 <- dt_01_E
    
    if (length(var_outlier) > 0){
      
      pb <- txtProgressBar(min = 1, max = max(length(var_outlier), 2), style = 3, width = 40)
      
      summary <- data.frame()
      
      for (i in 1:length(var_outlier)){
        
        var <- var_outlier[i]
        
        th1_     <- as.numeric(map_outlier[map_outlier$X1 == var, "X3"])
        th2_     <- as.numeric(map_outlier[map_outlier$X1 == var, "X4"])
        
        var_sym <- var %>% rlang::sym()
        
        condn <- map_na %>% 
          filter(X1 == var) %>% 
          pull(na_values)
        
        condn <- ifelse(is_empty(condn), "F", paste0(var, " %in% ", condn))
        
        dt_01_Octa9[, var] <- ifelse(dt_01_Octa9[, var] < th1_ | dt_01_Octa9[, var] > th2_, 99999999, dt_01_Octa9[, var])
        
        dt_01_E[, var] <- ifelse(dt_01_Octa9[, var] == 99999999, NA, dt_01_Octa9[, var])
        
        min_    <- min(dt_01_E[, var], na.rm = T) %>% suppressWarnings()
        mean_   <- mean(dt_01_E[, var], na.rm = T)
        median_ <- median(dt_01_E[, var], na.rm = T)
        max_    <- max(dt_01_E[, var], na.rm = T) %>% suppressWarnings()
        sd_     <- sd(dt_01_E[, var], na.rm = T)
        
        summary[i, "var"] <- var
        
        summary[i, "# Values"] <- dt_01_Octa9 %>% 
          filter(!is.na(!!var_sym)) %>%
          filter(var != "") %>% 
          nrow()
        
        summary[i, "# NAed (non-numeric)"] <- dt_01_D %>% 
          filter(stringr::str_detect(!!var_sym, "^[+-]?(\\d*\\.?\\d+|\\d+\\.?\\d*)$", negate = T), 
                 stringr::str_detect(!!var_sym, "^\\s*$", negate = T)) %>% 
          nrow()
        
        summary[i, "# NAed (NA proxies)"] <- dt_01_D %>% 
          filter(eval(parse(text=condn))) %>% 
          nrow()
        
        summary[i, "# NAed (threshold)"] <- dt_01_Octa9 %>% 
          filter(!!var_sym != 99999999) %>% 
          filter(!!var_sym < th1_ | !!var_sym > th2_) %>% 
          nrow()
        
        summary[i, "% NAed"] <- paste0(round(100*(summary[i, "# NAed (non-numeric)"] +
                                                    summary[i, "# NAed (NA proxies)"] +
                                                    summary[i, "# NAed (threshold)"])/summary[i, "# Values"], 2), " %")
        
        summary[i, "min"] <- round(min_, 2)
        summary[i, "mean"] <- round(mean_, 2)
        summary[i, "median"] <- round(median_, 2)
        summary[i, "max"] <- round(max_, 2)
        
        summary[i, "count below -3SD"] <- dt_01_Octa9 %>% 
          filter(!!var_sym != 99999999) %>% 
          filter(!!var_sym < mean_ - 3*sd_) %>% 
          nrow()
        
        summary[i, "count above +3SD"] <- dt_01_Octa9 %>% 
          filter(!!var_sym != 99999999) %>% 
          filter(!!var_sym > mean_ + 3*sd_) %>% 
          nrow()
        
        setTxtProgressBar(pb, i)
      }
      close(pb)
      
      if (nrow(summary) > 0) {
        map %>% 
          select(var = X1) %>% 
          left_join(summary, by = c("var")) %>% 
          mutate(across(everything(), ~ifelse(is.na(.), "-", as.character(.)))) %>% 
          write.table(file = file.path("temp.csv"), sep=",", col.names = F, row.names = F)
      }
    }
    
    # Save relevant dataset in local drive ----
    save(dt_01_E, file = "dt_01_E.Rda")
    save(dt_01_Octa9, file = "dt_01_Octa9.Rda")
    
    #====================================================
    
    # Log of run ----
    glue::glue("\n") %>% f_log_string(g_file_log)
    glue::glue("finished run in {round(Sys.time() - start_time, 0)} secs") %>% f_log_string(g_file_log)
    glue::glue("\n\n") %>% f_log_string(g_file_log)
    
    # clean and save environment in local drive ----
    rm(list = setdiff(ls(), ls(pattern = "^(d_|g_|f_)")))
    save.image(file=file.path(g_wd, "env.RData"))
    
    # Close the R code ----
    print(glue::glue("\n\nAll done!"))
    for(i in 1:3){
      print(glue::glue("Finishing in: {4 - i} sec"))
      Sys.sleep(1)
    }
  }, 
  
  warning = function(x){
    msg = glue::glue("{toString(x)}\n\ncheck code '{code_full}'")
    tcltk::tk_messageBox(type = c("ok"), msg, caption = "WARNING!", default = "", icon = "warning")
  },
  
  error = function(x){
    msg = glue::glue("{toString(x)}\n\ncheck code '{code_full}'")
    tcltk::tk_messageBox(type = c("ok"), msg, caption = "ERROR!", default = "", icon = "error")
  }
)

  