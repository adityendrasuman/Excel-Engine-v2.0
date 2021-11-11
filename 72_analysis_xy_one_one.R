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
  necessary.std = c("purrr", "dplyr", "rlang", "tidyselect", "tibble", "glue", "srvyr", "ggplot2"),
  necessary.github = c()
)
glue::glue("RUNNING R SERVER ...") %>% print()
glue::glue("Package status: {error}") %>% print()
glue::glue("\n") %>% print()

# Log of run ----
glue::glue("===================== Running '72_analyse_xy_one_one.R' =====================") %>% f_log_string(g_file_log) 
glue::glue("This analyses given y agianst given x variables, with default filters") %>% f_log_string(g_file_log)

#====================================================
question_creator <- function(query, i){
  
  # Get question row
  q <- query %>% 
    filter(row_number() == i)
  
  # Get summariser
  s <- q %>% 
    pull(5)
  
  # get y
  y <- q %>% 
    pull(3)
  
  # get x
  x <- q %>% 
    pull(4)
  
  if (is.na(x)){x <- character(0)}
  
  # get y label
  y_label <- q %>% 
    pull(6)
  
  # get x label
  x_label <- q %>% 
    pull(7)
  
  if (is.na(x_label)){x_label <- character(0)}

  condition <- d_skip %>% 
    filter(q_no == y) %>% 
    pull(condition)
  
  condition <- ifelse(is_empty(condition), "T", glue::glue("({trimws(condition)})"))
  
  question <- list(s, y, condition, x, x_label, y_label)
  return(question)
}

graph <- list()

if (args[2] == "all") {
  
  query <- f_read_xl(g_file_path, namedRegion = "xy_one_one_all", colNames = T, rowNames = F) %>% 
    filter(!is.na(sl)) %>% 
    filter(sl != "")
  
  pb <- txtProgressBar(min = 0, max = nrow(query), style = 3, width = 40)
  
  for (row in 1:nrow(query)){
    
    q <- query %>% 
      question_creator(row)
    
    answer <- d_02 %>% 
      f_answer_creator(q[[1]], q[[2]], q[[3]], q[[4]]) %>% 
      suppressWarnings()
    
    numeric_y = ifelse(class(d_02[[q[[2]]]]) == "numeric", T, F)
    
    graph[[row]] <- answer %>% 
      f_graph_1(q[[4]], q[[5]], q[[6]], q[[3]], numeric_y)
    
    setTxtProgressBar(pb, row)
    
  }
  
} else {
  
  row = 1
  
  json_str <- gsub("~", '"', args[2]) 
  query <- jsonlite::fromJSON(json_str) %>% 
    mutate_all(na_if,"")
  
  q <- query %>% 
    question_creator(row)
  
  answer <- d_02 %>% 
    f_answer_creator(q[[1]], q[[2]], q[[3]], q[[4]]) %>% 
    suppressWarnings()
  
  numeric_y = ifelse(class(d_02[[q[[2]]]]) == "numeric", T, F)
  
  graph[[row]] <- answer %>% 
    f_graph_1(q[[4]], q[[5]], q[[6]], q[[3]], numeric_y)
  
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
