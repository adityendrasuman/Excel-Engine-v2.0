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

# load librarise ----
error = f_libraries(
  necessary.std = c("dplyr", "stringr", "openxlsx", "rlang"),
  necessary.github = c()
)
glue::glue("RUNNING R SERVER ...") %>% print()
glue::glue("Package status: {error}") %>% print()
glue::glue("\n") %>% print()

# Log of run ----
glue::glue("===================== Running '10_incomplete_replace.R' =====================") %>% f_log_string(g_file_log)
glue::glue("This code replaces incomplete responses in the dataset indicated by the user") %>% f_log_string(g_file_log)

#====================================================

print(glue::glue("Picking mapping for incomplete responses from the excel interface..."))
map <- f_read_xl(g_file_path, namedRegion = "incomplete_R", colNames = F) %>% 
  filter(X3 != "~") %>% 
  filter(X3 != X2) %>% 
  unique() %>% 
  filter_all(any_vars(!is.na(.)))

d_01_C <- d_01_B 

if (nrow(map) == 0){
  print(glue::glue("Your input indicates no incomlete response"))
} else {
  print(glue::glue("Replacing incomplete responses..."))
  
  for (var in unique(map[, "X1"])){
    
    names <- map %>% 
      filter(X1 == var) %>% 
      select(X2) 
    
    for (name in names[[1]]){
      
      n_row_old <- d_01_C %>% 
        select(all_of(var)) %>% 
        filter(!!rlang::sym(var) == name) %>% 
        nrow()
      
      value <- map[map$X2 == name, "X3"]
      d_01_C[, var] <- ifelse(d_01_C[, var] == name, value, d_01_C[, var])
      
      n_row_old_after <- d_01_C %>% 
        select(all_of(var)) %>% 
        filter(!!rlang::sym(var) == name) %>% 
        nrow()
      
      n_row_new <- d_01_C %>% 
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

#====================================================

# Log of run ----
glue::glue("finished run in {round(Sys.time() - start_time, 0)} secs") %>% f_log_string(g_file_log)
glue::glue("\n\n") %>% f_log_string(g_file_log)

# remove unnecessary variables from environment ----
rm(list = setdiff(ls(), ls(pattern = "^(d_|g_|f_)")))

# save environment in a session temp variable ----
save.image(file=file.path(g_wd, "env.RData"))

# Close the R code
print(glue::glue("\n\nAll done!"))
for(i in 1:3){
  print(glue::glue("Finishing in: {4 - i} sec"))
  Sys.sleep(1)
}