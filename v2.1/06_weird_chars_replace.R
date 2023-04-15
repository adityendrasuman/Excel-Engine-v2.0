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
load("dt_01_A.Rda")

# load librarise ----
error = f_libraries(
  necessary.std = c("dplyr", "stringr", "openxlsx", "glue"),
  necessary.github = c()
)
glue::glue("RUNNING R SERVER ...") %>% print()
glue::glue("Package status: {error}") %>% print()
glue::glue("\n") %>% print()

# Log of run ----
glue::glue("===================== Running '06_weird_chars_replace.R' =====================")
glue::glue("This code attempts to remove unrecognised characters from the data, based on user suggestions in the excel interface")
glue::glue("\n") %>% f_log_string(g_file_log)

#====================================================

print(glue::glue("Picking mapping for weird characters from the excel interface..."))
map <- f_read_xl(g_file_path, namedRegion = "wc3_R", colNames = F) %>% 
  unique() %>% 
  filter_all(any_vars(!is.na(.))) %>%
  filter(!is.na(X1))

dt_01_B <- dt_01_A

i = 0
pb <- txtProgressBar(min = 1, max = ncol(dt_01_B), style = 3, width = 40)
print(glue::glue("Replacing weird characters..."))

for (var in colnames(dt_01_B)){
  for (name in map[,"X1"]){
    
    value <- map[map$X1 == name, "X2"]
      
    dt_01_B[, var] <- gsub(name, value, dt_01_B[, var])  
  }
  i = i + 1
  setTxtProgressBar(pb, i)
}
close(pb)

print(glue::glue("Double checking..."))
supplied_weird_chr <- f_read_xl(g_file_path, namedRegion = "wc1_R", colNames = F)
weird_chr <- paste(c("[^\x01-\x7F]", supplied_weird_chr[[1]]), collapse = "|")
summary <- f_id_char(dt_01_B, weird_chr)

if(is.null(nrow(summary))) {
  glue::glue("Any occurance of weird characters has been replaced") %>% f_log_string(g_file_log)
} else if(nrow(summary) > 0) {
  glue::glue("All occurances of weird characters could not be removed.") %>% f_log_string(g_file_log)
  glue::glue("Please check log file for the values that could not be removed") %>% f_log_string(g_file_log)
  glue::glue("Please remove manually in the raw data and upload it again") %>% f_log_string(g_file_log)
}

dt_01_C <- dt_01_B # IN CASE LIVE CAPTURE IS SKIPPED
#====================================================

# Log of run ----
if (!is.null(nrow(summary))){
  summary %>% f_log_table("List of Unrecognised Characters that could not be removed", g_file_log)
}

glue::glue("\n") %>% f_log_string(g_file_log)
glue::glue("finished run in {round(Sys.time() - start_time, 0)} secs") %>% f_log_string(g_file_log)
glue::glue("\n\n") %>% f_log_string(g_file_log)

# Save relevant datasets ----
save(dt_01_B, file = "dt_01_B.Rda")
save(dt_01_C, file = "dt_01_C.Rda")

# remove unnecessary variables from environment ----
rm(list = setdiff(ls(), ls(pattern = "^(d_|g_|f_)")))

# save environment in a session temp variable ----
save.image(file=file.path(g_wd, "env.RData"))

print(glue::glue("\n\nAll done!"))
for(i in 1:3){
  print(glue::glue("Finishing in: {4 - i} sec"))
  Sys.sleep(1)
}
