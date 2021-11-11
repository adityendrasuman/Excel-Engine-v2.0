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
cols_to_be_rectified <- names(d_01)[vapply(d_01, is.character, logical(1))]
d_01[,cols_to_be_rectified] <- lapply(d_01[,cols_to_be_rectified], trimws)
d_01[,cols_to_be_rectified] <- lapply(d_01[,cols_to_be_rectified], stringr::str_trim)

whitespaces <- paste(c("^\\s+.+\\s+$", ".+\\s+$", "^\\s+.+$"), collapse = "|")
summary <- f_id_char(d_01, whitespaces)

if(is.null(nrow(summary))) {
  glue::glue("Any leading or lagging white spaces has been removed") %>% f_log_string(g_file_log)
} else if(nrow(summary) > 0) {
  glue::glue("All white spaces could not be removed") %>% f_log_string(g_file_log)
  glue::glue("Please remove manually in the raw data and upload it again") %>% f_log_string(g_file_log)
}

#====================================================

# Log of run ----
glue::glue("\n") %>% f_log_string(g_file_log)
glue::glue("finished run in {round(Sys.time() - start_time, 0)} secs") %>% f_log_string(g_file_log)
glue::glue("\n\n") %>% f_log_string(g_file_log)

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