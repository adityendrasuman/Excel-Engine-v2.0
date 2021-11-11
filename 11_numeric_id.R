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
load("dt_01_C.Rda")

# load librarise ----
error = f_libraries(
  necessary.std = c("dplyr", "stringr", "glue"),
  necessary.github = c()
)
glue::glue("RUNNING R SERVER ...") %>% print()
glue::glue("Package status: {error}") %>% print()
glue::glue("\n") %>% print()

# Log of run ----
glue::glue("===================== Running '11_numeric_id.R' =====================") %>% f_log_string(g_file_log)
glue::glue("This code identifies columns in the data that have one or more full numeric response") %>% f_log_string(g_file_log)

#====================================================

summary <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(summary) <- c("variable", "value", "n")
pb <- txtProgressBar(min = 1, max = ncol(dt_01_C), style = 3, width = 40)
print(glue::glue("Checking columns for one or more numeric responses..."))
i = 0

for (var in colnames(dt_01_C)) {
  summary <- dt_01_C %>%
    select(all_of(var)) %>%
    group_by_all() %>% 
    count() %>% 
    as.data.frame() %>% 
    rename(value = 1) %>% 
    mutate(variable = var) %>% 
    rbind(summary)
  
  i = i + 1
  setTxtProgressBar(pb, i)
}
close(pb)

summary <- summary %>% 
  filter(stringr::str_detect(value, "^[+-]?(\\d*\\.?\\d+|\\d+\\.?\\d*)$")) %>%
  select(-n, -value) %>% 
  group_by_all() %>% 
  count() %>% 
  as.data.frame() %>% 
  select(variable)

summary %>% 
  mutate(is_numeric = "Yes",
         outlier_min = "~",
         outlier_max = "~",
         na_values = "~") %>% 
  write.table(file = file.path("temp.csv"), sep=",", col.names = F, row.names = F)

#====================================================

# Log of run ----
glue::glue("\n") %>% f_log_string(g_file_log)
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