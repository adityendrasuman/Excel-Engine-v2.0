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
if (args[6] == "refresh") {
  if (file.exists("env.RData")) {load("env.RData")}
}

# load custom functions ----
source(do.call(file.path, as.list(strsplit(paste0(args[2], "00_functions.R"), "\\|")[[1]])), 
       print.eval = TRUE, echo = F)

# load libraries ----
error = f_libraries(
  necessary.std = c("dplyr", "forcats", "gdata", "glue", "ggplot2", "gridExtra", 
                    "jsonlite", "openxlsx", "purrr", "profvis", "rlang", "srvyr", 
                    "stringr", "stats", "scales", "tidyselect", "tibble", "utils", 
                    "tidyr", "caret", "janitor", "e1071"),
  necessary.github = c()
)

glue::glue("RUNNING R SERVER ...") %>% print()
glue::glue("Package status: {error}") %>% print()
glue::glue("\n") %>% print()

# Log of run ----
if (args[6] == "refresh") {str = "Refresh: Loads the last saved environment and refreshes all the global variables using the latest interface"} 
if (args[6] == "reset") {str = "Reset: Initialises a blank environment with all the global variables using the latest interface"}

#====================================================

# global variables ----
g_excel_backend_temp_dir            <- do.call(file.path, as.list(strsplit(args[1], "\\|")[[1]]))
g_excel_backend_temp_nospace_dir_rf <- do.call(file.path, as.list(strsplit(args[2], "\\|")[[1]]))
g_excel_frontend_dir                <- do.call(file.path, as.list(strsplit(args[3], "\\|")[[1]]))
g_excel_backend_dir                 <- do.call(file.path, as.list(strsplit(args[4], "\\|")[[1]]))
g_file_name                         <- args[5]

g_file_path                         <- file.path(g_excel_frontend_dir, g_file_name)
g_wd                                <- g_excel_backend_temp_dir

g_tick                              <- "\u2713"
g_cross                             <- "\u2715"

g_file_log                          <- file.path(g_excel_frontend_dir, "Latest R logs.txt")
g_file_plot                         <- file.path(g_excel_frontend_dir, "Latest plots.pdf")

unlink(g_file_log)
unlink(g_file_plot)
#====================================================

# Log of run ----
glue::glue("===================== Running '01_initialise.R' =====================") %>% f_log_string(g_file_log) 
glue::glue("{str}")%>% f_log_string(g_file_log)
glue::glue("\n") %>% f_log_string(g_file_log)
glue::glue("finished run in {round(Sys.time() - start_time, 0)} secs") %>% f_log_string(g_file_log)
glue::glue("\n\n") %>% f_log_string(g_file_log)

# remove unnecessary variables from environment ----
rm(list = setdiff(ls(), ls(pattern = "^(d_|g_|f_)")))

# save environment in a session temp variable ----
save.image(file=file.path(g_wd, "env.RData"))
rm(d_01, d_01_A, d_01_B, d_01_C, d_01_D, d_01_Octa9)
save.image(file=file.path(g_wd, "env_small.RData"))

# Close the R code ----
print(glue::glue("\n\nAll done!"))
for(i in 1:3){
  print(glue::glue("Finishing in: {4 - i} sec"))
  Sys.sleep(1)
}