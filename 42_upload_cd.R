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
  necessary.std = c("openxlsx", "glue", "dplyr"),
  necessary.github = c()
)
glue::glue("RUNNING R SERVER ...") %>% print()
glue::glue("Package status: {error}") %>% print()
glue::glue("\n") %>% print()

# Log of run ----
glue::glue("===================== Running '42_upload_cd.R' =====================") %>% f_log_string(g_file_log) 
glue::glue("This code uploads newly created columns into the R environment and deletes datasets from the cleanup stage") %>% f_log_string(g_file_log)

#====================================================

print(glue::glue("Importing clean data ..."))
d_02 <- f_read_xl(g_file_path, namedRegion = "cd_all", colNames = T)
print(glue::glue("Imported data has {ncol(d_02)} columns and {nrow(d_02)} rows"))

#====================================================

# Log of run ----
glue::glue("finished run in {round(Sys.time() - start_time, 0)} secs") %>% f_log_string(g_file_log)
glue::glue("\n") %>% f_log_string(g_file_log)

# remove unnecessary variables from environment ----
rm(list = setdiff(ls(), ls(pattern = "^(d_|g_|f_)")))

# save environment in a session temp variable ----
save.image(file=file.path(g_wd, "env.RData"))
rm(d_01, d_01_A, d_01_B, d_01_C, d_01_D, d_01_Octa9)
save.image(file=file.path(g_wd, "env_small.RData"))

# Close the R code
print(glue::glue("\n\nAll done!"))
for(i in 1:3){
  print(glue::glue("Finishing in: {4 - i} sec"))
  Sys.sleep(1)
}
