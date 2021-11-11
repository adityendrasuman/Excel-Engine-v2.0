# cleanup the environment ----
rm(list = ls())
if (!is.null(dev.list())) dev.off()
options(survey.lonely.psu="adjust")
cat("\014")
start_time <- Sys.time()

# capture variable coming from vba ----
args <- commandArgs(trailingOnly=T)

# set working director ----
setwd(do.call(file.path, as.list(strsplit(args[1], "\\|")[[1]])))

# load environment ----
load("env_small.RData")

source(file.path(g_excel_backend_temp_nospace_dir_rf, "00_functions.R"))

# load libraries ----
error = f_libraries(
  necessary.std = c("dplyr", "rlang", "stats", "ggplot2", "scales", "forcats", "jsonlite", "stringr"),
  necessary.github = c()
)
glue::glue("RUNNING R SERVER ...") %>% print()
glue::glue("Package status: {error}") %>% print()
glue::glue("\n") %>% print()

# Log of run ----
glue::glue("===================== Running '74_analyse_xy_rtree.R' =====================") %>% f_log_string(g_file_log) 
glue::glue("This creates Regression Tree for y agianst x variables, with custom filters") %>% f_log_string(g_file_log)

#====================================================
question_creator <- function(data, i){
  
  df <- data %>% 
    filter(X1 == i)
  
  # get summariser
  s <- df %>% 
    filter(Var.type == "SUMMARISER") %>% 
    pull(Variable)
  
  # get y
  y <- df %>% 
    filter(Var.type == "[Y]") %>% 
    pull(Variable)
  
  # get xs
  x <- df %>% 
    filter(Var.type == "[X]") %>% 
    pull(Variable)
  
  # get y label
  y_label <- df %>%
    filter(Var.type == "[Y]") %>% 
    pull(Description)
  
  # get x label
  x_label <- df %>%
    filter(Var.type == "[X]") %>% 
    pull(Description)
  
  # get condition
  skip_filtered_for_q <- df %>% 
    filter(Var.type == "FILTER") %>% 
    select(Variable, condition_sign, condition_value)
  
  # number of condition variables
  num_conditions <- skip_filtered_for_q %>% 
    nrow()
  
  # initialise condition text
  condition <- d_skip %>% 
    filter(q_no == y) %>% 
    pull(condition)
  
  condition <- ifelse(is_empty(condition), "T", glue::glue("({trimws(condition)})"))
  
  # for each condition ...
  if (num_conditions > 0){
    
    if (condition == "T") {
      condition <- ""
    } else {
      condition <- glue::glue("{condition} & ")
    }
    
    for (i in 1:num_conditions){
      
      # get condition variable 
      var <- skip_filtered_for_q[i, "Variable"]
      
      # get relation between condition variable and the values
      sign <- skip_filtered_for_q[i, "condition_sign"]
      
      # get all allowed values of condition variable, i.e. response vector
      response_vector <- skip_filtered_for_q[i, "condition_value"] %>% 
        strsplit(split = "\\|") %>% 
        gdata::trim() %>% 
        unlist()
      
      # calculate size of this response vector
      num_response <- length(response_vector)
      
      # check if response vector is numeric or charecter
      response_is_string <- response_vector %>% 
        as.numeric() %>% 
        is.na() %>% 
        suppressWarnings() %>% 
        sum()
      
      if (num_response > 1){
        if (response_is_string > 0){
          
          # if more than one response in string format, create c("a", "b", "c")
          str <- paste(response_vector, collapse = '", "')
          response_string <- glue::glue('c("{str}")') 
        } else {
          
          # if more than one response in numeric format, create c(1, 3, 5, 9) 
          str <- paste(response_vector, collapse = ', ')
          response_string <- glue::glue('c({str})')
        }
      } else {
        
        # if a single response ...
        if (sign == "not in") {sign = "!="}
        if (sign == "in") {sign = "=="}
        if (response_is_string > 0){
          
          # ... in string format, create "a"
          str <- response_vector[1]
          response_string <- glue::glue('"{str}"')
        } else {
          
          # ... in numeric format, create 1
          str <- response_vector[1]
          response_string <- glue::glue('{str}')
        }
      }
      
      # apend to previous condition and make it redy to append the condition string using the "next condition" string
      next_condn <- ifelse(i == num_conditions, "", " & ")
      
      if (sign == "not in") {
        condition <- glue::glue("{condition} !({var} %in% {response_string}){next_condn}")
      } else if(sign == "in") {
        condition <- glue::glue("{condition} {var} %in% {response_string}{next_condn}")
      } else {
        condition <- glue::glue("{condition} {var} {sign} {response_string}{next_condn}")
      }
    }
  }
  
  question <- list(s, y, condition, x, x_label, y_label)
  return(question)
}

if (args[2] == "all"){
  data <- f_read_xl(g_file_path, namedRegion = "xy_rtree_all_temp", colNames = T, rowNames = F)
} else {
  json_str <- gsub("~", '"', args[2]) 
  data <- jsonlite::fromJSON(json_str) %>% 
    mutate_all(na_if,"")
  colnames(data) <- gsub(" ", ".", colnames(data)) 
}

data <- data %>% 
  dplyr::filter(!grepl("^ANALYSIS ",X1)) %>% 
  mutate(X1 = as.numeric(X1)) %>% 
  filter_all(any_vars(!is.na(.))) %>% 
  mutate(
    condition_sign = case_when(
      Var.type == "FILTER" ~ lead(Description)),
    condition_value = case_when(
      Var.type == "FILTER" ~ lead(X6))
  ) %>% 
  filter(!is.na(Var.type)) %>% 
  mutate(sl = row_number()) %>% 
  mutate(X1 = as.numeric(X1)) %>% 
  select(-RUN, -X6) %>% 
  select(sl, everything())

data$X1 <- cumsum(!is.na(data$X1))

# Remove blank X variables
data <- data %>% 
  filter(!Var.type %in% c("[X]", "FILTER") | !is.na(Variable))

# Remove analysis cards that have blank Y variable
to_delete <- data %>% 
  filter(Var.type == "[Y]", is.na(Variable)) %>% 
  pull(X1)

if (length(to_delete) > 0) {
  data <- data %>%
    filter(X1 != to_delete)
}

graph <- list()

if (args[2] == "all") {
  
  pb <- txtProgressBar(min = 0, max = max(data$X1), style = 3, width = 40)
  
  for (q_no in unique(data$X1)){
    
    q <- data %>% 
      question_creator(q_no)
    
    answer_actual_sample <- d_02 %>% 
      f_segmentor(y = q[[2]], x = q[[4]], s = q[[1]], y_desc = q[[5]], x_desc = q[[6]], 
                  forced_sample = F, ignore_weight_responses = c())

    answer_forced_sample <- d_02 %>% 
      f_segmentor(y = q[[2]], x = q[[4]], s = q[[1]], y_desc = q[[5]], x_desc = q[[6]],
                  forced_sample = T, ignore_weight_responses = c())

    setTxtProgressBar(pb, q_no)
  }  
    
} else {
  
  q_no <- data %>% 
    slice(1) %>% 
    pull(X1)
  
  q <- data %>% 
    question_creator(q_no)
  
  answer_forced_sample <- d_02 %>% 
    f_segmentor(y = q[[2]], x = q[[4]], s = q[[1]], y_desc = q[[5]], x_desc = q[[6]],
                forced_sample = T, ignore_weight_responses = c())
  
  answer_actual_sample <- d_02 %>% 
    f_segmentor(y = q[[2]], x = q[[4]], s = q[[1]], y_desc = q[[5]], x_desc = q[[6]], 
                forced_sample = F, ignore_weight_responses = c())
  
}
  


graph %>% 
  f_plotter(g_excel_frontend_dir)




#====================================================

# Log of run ----
glue::glue("finished run in {round(Sys.time() - start_time, 0)} secs") %>% f_log_string(g_file_log)
glue::glue("\n\n") %>% f_log_string(g_file_log)

# remove unnecessary variables from environment ----
rm(list = setdiff(ls(), ls(pattern = "^(d_|g_|f_)")))

# save environment in a session temp variable ----
save.image(file=file.path(g_wd, "env.RData"))

print(glue::glue("\n\n All done!"))
for(i in 1:3){
  print(glue::glue("Finishing in: {4 - i} sec"))
  Sys.sleep(1)
}
