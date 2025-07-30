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
    load("dt_01_C.Rda")
    
    # load librarise ----
    error = f_libraries(
      necessary.std = c("dplyr", "stringr", "openxlsx", "rlang"),
      necessary.github = c()
    )
    glue::glue("RUNNING R SERVER ...") %>% print()
    glue::glue("Package status: {error}") %>% print()
    glue::glue("\n") %>% print()
    
    # Code specific inputs ----
    purpose <- "Replacing incomplete responses in the dataset as indicated by the user"
    
    code_full <- scriptName::current_filename()
    code_path <- ifelse(is.null(code_full), "", dirname(code_full)) 
    code_name <- ifelse(is.null(code_full), "", basename(code_full))
    
    # Log of run ----
    glue::glue("===================== Running '{code_name}' =====================") %>% f_log_string(g_file_log) 
    glue::glue("{purpose}")%>% f_log_string(g_file_log)
    glue::glue("\n") %>% f_log_string(g_file_log)

    #====================================================
    
    print(glue::glue("Picking mapping for incomplete responses from the excel interface..."))
    
    df_temp <- f_read_xl(g_file_path, namedRegion = "incomplete_R", colNames = F) %>% 
      suppressWarnings()
    
    if (is.null(df_temp) || nrow(df_temp) == 0){
      map <- data.frame(X1 = character(0), X2 = character(0), X3=character(0), stringsAsFactors = FALSE)
    } else {
      map <- df_temp %>%
        filter(X3 != "~") %>% 
        filter(X3 != X2) %>% 
        unique() %>% 
        filter_all(any_vars(!is.na(.)))
    }
    
    dt_01_D <- dt_01_C 
    
    if (nrow(map) == 0){
      print(glue::glue("Your input indicates no incomlete response"))
    } else {
      print(glue::glue("Replacing incomplete responses..."))
      
      for (var in unique(map[, "X1"])){
        
        names <- map %>% 
          filter(X1 == var) %>% 
          select(X2) 
        
        for (name in names[[1]]){
          
          n_row_old <- dt_01_D %>% 
            select(all_of(var)) %>% 
            filter(!!rlang::sym(var) == name) %>% 
            nrow()
          
          value <- map[map$X2 == name, "X3"]
          dt_01_D[, var] <- ifelse(dt_01_D[, var] == name, value, dt_01_D[, var])
          
          n_row_old_after <- dt_01_D %>% 
            select(all_of(var)) %>% 
            filter(!!rlang::sym(var) == name) %>% 
            nrow()
          
          n_row_new <- dt_01_D %>% 
            select(all_of(var)) %>% 
            filter(!!rlang::sym(var) == value) %>% 
            nrow()
          
          if (n_row_new == n_row_old & n_row_old_after == 0){
            print(glue::glue("Replaced {n_row_new} instances of '{name}' with '{value}'"))
          }
          
          if (n_row_new != n_row_old | n_row_old_after > 0){
            print(glue::glue("Could NOT replace '{name}' with '{value}'"))
          }
        }
      }
    }
    
    # save relevant dataset ----
    save(dt_01_D, file = "dt_01_D.Rda")
    
    #====================================================
    
    # Log of run ----
    glue::glue("\n") %>% f_log_string(g_file_log)
    glue::glue("finished run in {round(Sys.time() - start_time, 0)} secs") %>% f_log_string(g_file_log)
    glue::glue("\n\n") %>% f_log_string(g_file_log)
    
    # clean and save environment in a session temp variable ----
    rm(list = setdiff(ls(), ls(pattern = "^(d_|g_|f_)")))
    save.image(file=file.path(g_wd, "env.RData"))
    
    # Close the R code
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