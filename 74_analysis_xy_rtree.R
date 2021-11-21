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
load("env.RData")
load("dt_02.Rda")

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
question_creator <- function(card){
  
  df <- card
  
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
  
  # notes
  notes <- df %>% 
    filter(Notes != "" & !is.na(Notes)) %>% 
    pull(Notes)
  
  condition <- ifelse(rlang::is_empty(condition), "T", glue::glue("({trimws(condition)})"))
  
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
  
  question <- list(s, y, condition, x, x_label, y_label, notes)
  return(question)
}

if (args[2] == "section"){
  data <- f_read_xl(g_file_path, namedRegion = "xy_custom_all_temp", colNames = T, rowNames = F)
  section <- args[3]
  row_start <- which(data$X1 == paste0("SECTION ", stringr::str_pad(section, 2, pad = "0"), ": "))
  row_end <- which(data$X1 == paste0("SECTION ", stringr::str_pad(as.numeric(section) + 1, 2, pad = "0"), ": "))
  if (length(row_end) == 0) {row_end <- nrow(data) + 1}
  
  data <- data %>% 
    slice(row_start:(row_end - 1))
  
} else if (args[2] == "all"){
  data <- f_read_xl(g_file_path, namedRegion = "xy_custom_all_temp", colNames = T, rowNames = F)

} else {
  json_str <- gsub("~", '"', args[2]) 
  data <- jsonlite::fromJSON(json_str) %>% 
    dplyr::mutate_all(dplyr::na_if,"")
  colnames(data) <- gsub(" ", ".", colnames(data)) 
}

data <- data %>%
  mutate(Var.type = case_when(
    substr(X1, 1, 7) == "SECTION" ~ paste(X1, Var.type),
    T ~ Var.type)) %>%
  mutate(X1 = case_when(
    substr(X1, 1, 7) == "SECTION" ~ lead(X1),
    T ~ X1)) %>% 
  mutate(X1 = as.numeric(X1)) %>% 
  filter_all(any_vars(!is.na(.))) %>% 
  mutate(
    condition_sign = case_when(
      Var.type == "FILTER" ~ lead(Description)),
    
    condition_value = case_when(
      Var.type == "FILTER" ~ lead(X6)),
    
    show_condition_sign = case_when(
      Var.type == "SHOW" ~ lead(Description)),
    
    show_condition_value = case_when(
      Var.type == "SHOW" ~ lead(X6))) %>%
  
  filter(!is.na(Var.type)) %>% 
  mutate(sl = row_number()) %>% 
  mutate(X1 = as.numeric(X1)) %>% 
  select(-X6) %>% 
  select(sl, everything())

data$X1 <- cumsum(!is.na(data$X1))

# Remove analysis cards that have blank Y variable
to_delete <- data %>% 
  filter(Var.type == "[Y]", is.na(Variable)) %>% 
  pull(X1)

if (length(to_delete) > 0) {
  data <- data %>%
    filter(X1 != to_delete)
}

# Remove blank X AND FILTER variables
data <- data %>% 
  filter(!(Var.type %in% c("[X]", "FILTER") & (is.na(Variable))))

graph <- list()
pb <- txtProgressBar(min = min(data$X1), max = max(max(data$X1), min(data$X1) + 1), style = 3, width = 40)
card_num <- 0

for (q_no in unique(data$X1)){
  
  each_card <- data %>% 
    filter(X1 == q_no)
  
  section_name <- each_card %>% 
    slice(1) %>% 
    pull(Var.type)
  
  is_section = ifelse(substr(section_name,1,7) == "SECTION", T, F)
  
  if (is_section){
    
    graph[[length(graph) + 1]] <- section_name %>%
      f_graph_section()
    
  } else {
    
    card_num <- card_num + 1
    
    xxx <- tryCatch(
      {
        each_card <- each_card %>% 
          select(-show_condition_sign, -show_condition_value)
        
        # For each card create list of questions 
        q <- each_card %>% 
            question_creator()
        
        # For each question, create answers and rbind them
        # y_label <- d_colmap %>%
        #   filter(X1 == q[[2]]) %>%
        #   pull(X2)
        y_label <- character(0)
          
        if (length(q[[7]]) != 0) {y_label <- paste0(q[[7]], " | ", y_label)}
        if(length(y_label) == 0) {y_label = "Label could not be loaded - please re-run colnames upload"}
        
        graph[[length(graph) + 1]] <- dt_02 %>%
          f_segmentor(s = q[[1]], y_in = q[[2]], filter_in = q[[3]], x_all_in = q[[4]], file = y_label)
        
        graph[[length(graph) + 1]] <- dt_02 %>%
          f_segmentor(s = q[[1]], y_in = q[[2]], filter_in = q[[3]], x_all_in = q[[4]], file = y_label, with_weight = T)
        
        setTxtProgressBar(pb, q_no)
      },
      
      error = function(e){
        (glue::glue("\n !ERROR: At card number {card_num} of the selected card set")) %>% 
          f_log_string(g_file_log) 
        
        card_num %>% 
          f_graph_error1() %>% 
          return()
      }
    ) # END OF OUTER TRY CATCH
    if (class(xxx)[1] == "gg"){graph[[length(graph) + 1]] <- xxx}  
    
  }
}

graph %>% 
  f_plotter(g_excel_frontend_dir)


#====================================================

# Log of run ----
glue::glue("\n") %>% f_log_string(g_file_log)
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
