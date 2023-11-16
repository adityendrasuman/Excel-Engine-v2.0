# INSTRUCTIONS:
# CREATE COLUMNS TO BE ADDED INTO THE CLEANED DATA HERE 

# ************* For manual run, Run this part ***************
# cleanup the environment ----
rm(list = ls())
if (!is.null(dev.list())) dev.off()
cat("\014")
start_time <- Sys.time()

library(rstudioapi)
file_loc <- dirname(rstudioapi::getActiveDocumentContext()$path)
load(file.path(file_loc,"env.RData"))
load(file.path(file_loc,"dt_01_D.Rda"))

error = f_libraries(
  necessary.std = c("dplyr", "forcats", "gdata", "glue", "ggplot2", "gridExtra", 
                    "jsonlite", "openxlsx", "purrr", "profvis", "rlang", "srvyr", 
                    "stringr", "stats", "scales", "tidyselect", "tibble", "utils", 
                    "tidyr", "caret", "janitor", "e1071", "rpart", "rpart.plot"),
  necessary.github = c()
)
df_in <- dt_01_D
# ******************** End of manual run *********************

# ##########################################################################
# DO NOT ADD ANY NEW LINE TILL HERE. YOUR CODE SHOULD START AFTER LINE 45
# ##########################################################################
add_to_skip <- function(data, y, ...){
  data <- data %>% 
    rbind(data.frame(new = y, old = c(...)))
}

create_new_col <- function(df_in){
  
  df_out <- df_in
  
  # Container to hold skip logic for additional columns 
  df_skip_info <- data.frame(matrix(ncol=2, nrow=0))
  colnames(df_skip_info) <- c("new", "old")
  
  # ++++++++++++++++++++++++++START OF SPACE FOR DEFINING NEW COLUMNS++++++++++++++++++++++++++

  #000a: <Column name> ---- 
  
  # STEP 1: Define the column
  # STEP 2: Check if the column is correctly created
  # STEP 3: Apply the skip logic on new column based on combination of existing columns using (, &, |
    # example a: add_to_skip("new_col", "T") when no skip logic needs to be assigned
    # example b: add_to_skip("new_col", "col1", "col2") intersection of two columns is required
    # example c: add_to_skip("new_col", "col1 & (col2 | col3)") when a more complex combination is required
  
  #000b: (illustration - don't delete) Unit Weight ----
  
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
  df_skip_info <- df_skip_info %>%
    add_to_skip ("z_unit_weight", "T")
  
  # 001: <Description> ----

  # 002: <Description> ----

  # ++++++++++++++++++++++++++ END OF SPACE FOR DEFINING NEW COLUMNS ++++++++++++++++++++++++++
  return (list(df_out, df_skip_info))
}



