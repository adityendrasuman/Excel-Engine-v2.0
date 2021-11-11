rm(list = ls())
if (!is.null(dev.list())) dev.off()
cat("\014")
start_time <- Sys.time()

# capture variable coming from vba ----
args <- commandArgs(trailingOnly=T)

# set working director ---- 
setwd(do.call(file.path, as.list(strsplit(args[1], "\\|")[[1]])))

# load environment ----
load("env_small.RData")

# load librarise ----
error = f_libraries(
  necessary.std = c("dplyr", "glue"),
  necessary.github = c()
)
glue::glue("RUNNING R SERVER ...") %>% print()
glue::glue("Package status: {error}") %>% print()
glue::glue("\n") %>% print()

# Log of run ----
glue::glue("===================== Running '43_colnames.R' =====================")
glue::glue("Gets all the column names from the data into the interface")
glue::glue("\n") %>% f_log_string(g_file_log)
#====================================================

d_02 %>%
  colnames() %>%
  write.table(file = file.path("temp.csv"), sep=",", col.names = F, row.names = F)

#====================================================

# Log of run ----
glue::glue("finished run in {round(Sys.time() - start_time, 0)} secs") %>% f_log_string(g_file_log)
glue::glue("\n\n") %>% f_log_string(g_file_log)

# remove unnecessary variables from environment ----
rm(list = setdiff(ls(), ls(pattern = "^(d_|g_|f_)")))

# save environment in a session temp variable ----
# save.image(file=file.path(g_wd, "env.RData"))
# rm(d_01, d_01_A, d_01_B, d_01_C, d_01_D, d_01_Octa9)
# save.image(file=file.path(g_wd, "env_small.RData"))

print(glue::glue("\n\nAll done!"))
for(i in 1:3){
  print(glue::glue("Finishing in: {4 - i} sec"))
  Sys.sleep(1)
}