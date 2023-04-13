create_new_col <- function(df_in){
  
  # MANUAL RUN FLAG #########################################################################
  manual_run = F    # IF RUNNING MANUALLY ENSURE THAT 'manual_run' IS SET TO 'T' ELSE 'F'
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  if (manual_run == T) {
    
    # cleanup the environment
    rm(list = ls())
    if (!is.null(dev.list())) dev.off()
    cat("\014")
    start_time <- Sys.time()
    
    # get location and load relevant files
    library(rstudioapi)
    file_loc <- dirname(rstudioapi::getActiveDocumentContext()$path)
    load(file.path(file_loc,"env.RData"))
    load(file.path(file_loc,"dt_01_D.Rda"))
    
    # load libraries
    error = f_libraries(
      necessary.std = c("dplyr", "forcats", "gdata", "glue", "ggplot2", "gridExtra", 
                        "jsonlite", "openxlsx", "purrr", "profvis", "rlang", "srvyr", 
                        "stringr", "stats", "scales", "tidyselect", "tibble", "utils", 
                        "tidyr", "caret", "janitor", "e1071", "rpart", "rpart.plot"),
      necessary.github = c()
    )
    
    df_in <- dt_01_D
  }
  
  # prepare container and dataset for skip logic
  add_to_skip <- function(data, y, ...){
    data <- data %>% 
      rbind(data.frame(new = y, old = c(...)))
  }
  
  df_skip_info <- data.frame(matrix(ncol=2, nrow=0))
  colnames(df_skip_info) <- c("new", "old")
  
  df_out <- df_in
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # START OF SPACE FOR DEFINING COLUMNS
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # 000: Unit Weight ----
  
    # STEP 1: Define the column
    df_out <- df_out %>% 
      mutate(
        z_unit_weight = 1
      )
    
    # STEP 2: Check if the column is correctly created
    df_out %>% 
      select(z_unit_weight) %>% 
      f_grouper()
    
    # STEP 3: Apply the skip logic on new column based on combination of existing columns using '(', '&', '|'
    # example a: add_to_skip("new_col", "T") when no skip logic needs to be assigned
    # example b: add_to_skip("new_col", "col1", "col2") intersection of two columns is required
    # example c: add_to_skip("new_col", "col1 & (col2 | col3)") when a more complex combination is required
    
    df_skip_info <- df_skip_info %>%
      add_to_skip ("z_unit_weight", "state", "dist")
  
  # 000: <Column name> ----
  
    # STEP 1: Define the column
  
    # STEP 2: Check if the column is correctly created
  
    # STEP 3: Apply the skip logic on new column based on combination of existing columns using (, &, |

  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # END OF SPACE FOR DEFINING COLUMNS
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  return (list(df_out, df_skip_info))
}



