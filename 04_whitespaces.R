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
load("dt_01.Rda")

# load libraries ----
error = f_libraries(
  necessary.std = c("dplyr", "stringr", "profvis", "glue"),
  necessary.github = c()
)
glue::glue("RUNNING R SERVER ...") %>% print()
glue::glue("Package status: {error}") %>% print()
glue::glue("\n") %>% print()

# Log of run ----
glue::glue("===================== Running '04_whitespaces.R' =====================") %>% f_log_string(g_file_log)
glue::glue("This code trims responses for whitespaces around them") %>% f_log_string(g_file_log)
glue::glue("\n") %>% f_log_string(g_file_log)

#====================================================

print(glue::glue("Trimming whitespaces..."))
cols_to_be_rectified <- names(dt_01)[vapply(dt_01, is.character, logical(1))]
dt_01[,cols_to_be_rectified] <- lapply(dt_01[,cols_to_be_rectified], trimws)
dt_01[,cols_to_be_rectified] <- lapply(dt_01[,cols_to_be_rectified], stringr::str_trim)

whitespaces <- paste(c("^\\s+.+\\s+$", ".+\\s+$", "^\\s+.+$"), collapse = "|")
summary <- f_id_char(dt_01, whitespaces)

if(is.null(nrow(summary))) {
  glue::glue("All leading or lagging white spaces have been removed") %>% f_log_string(g_file_log)
} else if(nrow(summary) > 0) {
  glue::glue("All white spaces could not be removed") %>% f_log_string(g_file_log)
  glue::glue("Please check log file for the values that could not be removed") %>% f_log_string(g_file_log)
}

#====================================================

# Log of run ----
if (!is.null(nrow(summary))){
  summary %>% f_log_table("List of White spaces that could not be removed", g_file_log)
}

glue::glue("\n") %>% f_log_string(g_file_log)
glue::glue("finished run in {round(Sys.time() - start_time, 0)} secs") %>% f_log_string(g_file_log)
glue::glue("\n\n") %>% f_log_string(g_file_log)

# Save relevant datasets ----
save(dt_01, file = "dt_01.Rda")
 
# remove unnecessary variables from environment ----
rm(list = setdiff(ls(), ls(pattern = "^(d_|g_|f_)")))

# save environment in a session temp variable ----
save.image(file=file.path(g_wd, "env.RData"))

# Close the R code ----
print(glue::glue("\n\nAll done!"))
for(i in 1:3){
  print(glue::glue("Finishing in: {4 - i} sec"))
  Sys.sleep(1)
}