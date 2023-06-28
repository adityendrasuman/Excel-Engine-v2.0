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

#======================= Write all analysis below this =========================

 #Col Definitions
df_in <- df_in %>% 
  mutate(r_age = case_when(
    a2 %in% c("18-19","20-24") ~ "<45",
    a2 %in% c("25-29","30-34") ~ "<45",
    a2 %in% c("35-39","40-44") ~ "<45",
    a2 %in% c("45-49","50-54","55-59") ~ ">45",
    a2 %in% c("60-64","65-69","70-74","75-79","80+") ~ ">45",
    T ~ NA_character_
  ))



#======================= End of Col Codes =========================

dt_03 <- df_in

save(dt_03, file = "dt_03.Rda")
