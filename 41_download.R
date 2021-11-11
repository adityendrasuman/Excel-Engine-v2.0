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
load("dt_01_D.Rda")

# load librarise ----
error = f_libraries(
  necessary.std = c("dplyr", "glue"),
  necessary.github = c()
)
glue::glue("RUNNING R SERVER ...") %>% print()
glue::glue("Package status: {error}") %>% print()
glue::glue("\n") %>% print()

# Log of run ----
glue::glue("===================== Running '41_download.R' =====================") %>% f_log_string(g_file_log) 
glue::glue("Imports cleaned-up data from the R environment into the interface") %>% f_log_string(g_file_log)

#====================================================
file_name <- file.path(g_excel_backend_temp_dir, "create_new_columns_in_r.R")
start <- 23
file.lines <- scan(file_name, what=character(), skip=start-1, sep='\n', quiet = T) 
file.lines.collapsed <- paste(file.lines, collapse='\n')
source(textConnection(file.lines.collapsed), print.eval = TRUE, echo = F)

d <- create_new_col(dt_01_D)
df_out <- d[[1]]
d_skip_newcol <- d[[2]]

df_out %>%
  write.table(file = file.path("temp.csv"), sep=",", col.names = T, row.names = F)

print(glue::glue("Data is being prepared for importing into the interface. Please wait ..."))
#====================================================

# Log of run ----
glue::glue("finished run in {round(Sys.time() - start_time, 0)} secs") %>% f_log_string(g_file_log)
glue::glue("\n") %>% f_log_string(g_file_log)

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
