# cleanup the environment
rm(list = ls())
if (!is.null(dev.list())) dev.off()
cat("\014")
start_time <- Sys.time()

# get location and load relevant files
library(rstudioapi)
file_loc <- dirname(rstudioapi::getActiveDocumentContext()$path)
load(file.path(file_loc,"env.RData"))
load(file.path(file_loc,"dt_02.Rda"))

# load libraries
error = f_libraries(
  necessary.std = c("dplyr", "forcats", "gdata", "glue", "ggplot2", "gridExtra", 
                    "jsonlite", "openxlsx", "purrr", "profvis", "rlang", "srvyr", 
                    "stringr", "stats", "scales", "tidyselect", "tibble", "utils", 
                    "tidyr", "caret", "janitor", "e1071", "rpart", "rpart.plot"),
  necessary.github = c()
)

df_in <- dt_02

#======================= Write all columns below this =========================

#======================= End of Col Codes =========================

dt_03 <- df_in

save(dt_03, file = "dt_03.Rda")
