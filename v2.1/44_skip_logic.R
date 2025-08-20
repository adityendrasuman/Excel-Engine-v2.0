tryCatch(
  {
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
    load("dt_02.Rda")
    
    # load librarise ----
    error = f_libraries(
      necessary.std = c("dplyr", "glue", "gdata"),
      necessary.github = c()
    )
    glue::glue("RUNNING R SERVER ...") %>% print()
    glue::glue("Package status: {error}") %>% print()
    glue::glue("\n") %>% print()
    
    # Code specific inputs ----
    purpose <- "Uploading and analysing skip logic"
    
    code_full <- scriptName::current_filename()
    code_path <- ifelse(is.null(code_full), "", dirname(code_full)) 
    code_name <- ifelse(is.null(code_full), "", basename(code_full))
    
    # Log of run ----
    glue::glue("===================== Running '{code_name}' =====================") %>% f_log_string(g_file_log) 
    glue::glue("{purpose}")%>% f_log_string(g_file_log)
    glue::glue("\n") %>% f_log_string(g_file_log)
    
    #====================================================
    
    df_temp <- f_read_xl(g_file_path, namedRegion = "body_skip", colNames = F) %>% 
      suppressWarnings()
    
    if (is.null(df_temp) || nrow(df_temp) == 0){
      map <- data.frame(X1 = character(0), X2 = character(0), X3=character(0), X4 = character(0), X5=character(0), stringsAsFactors = FALSE)
    } else {
      
      if (ncol(df_temp) == 3) {
        map <- df_temp %>% 
          mutate(X4 = NA_character_) %>% 
          mutate(X5 = "")
      } else if (ncol(df_temp) == 4) {
        map <- df_temp %>% 
          mutate(X5 = "")
      } else {
        map <- df_temp
      }
      
      map <- map %>% 
        select(check_var_regex = X1,
               condition_var = X2,
               sign	= X3,
               response = X4,
               next_condition = X5)
      
      
      # Regex-question mapping
      regex_q_mapping <- data.frame(matrix(ncol=2, nrow=0))
      colnames(regex_q_mapping) <- c("check_var_regex", "check_var")
      
      # Overall Summary for export ...
      skip_logic_log <- data.frame(matrix(ncol=8, nrow=0))
      colnames(skip_logic_log) <- c("var_to_be_checked", "total_rows", "num_values", 
                                    "rows_that_satisfy_condition", "num_violations",
                                    "value_when_condition_unmet", "blank_when_condition_met",
                                    "condition")
      
      # Overall Summary for analysis filters ...
      d_skip <- data.frame(matrix(ncol=2, nrow=0))
      colnames(d_skip) <- c("q_no", "condition")
      
      # Figure out all the regex strings to apply the check on
      questions_regex <- map %>% 
        pull(check_var_regex) %>% 
        unique()
      
      grep_vars2 <- function (needle, haystack, ...) {grep(needle, haystack, perl = TRUE, ...)}
      matches2 <- function (match, ignore.case = TRUE, vars = tidyselect::peek_vars()) {grep_vars2(match, vars, ignore.case = ignore.case)}
      
      for (q_regex in questions_regex) {
        regex_q_mapping <- dt_02 %>% 
          select(matches2(q_regex)) %>%  
          colnames() %>% 
          as.data.frame() %>% 
          select(check_var = 1) %>% 
          mutate(check_var_regex = q_regex) %>% 
          select(check_var_regex, check_var) %>% 
          rbind(regex_q_mapping)
      }
      
      # Add question number to the skip logic map
      map <- map %>% 
        left_join(regex_q_mapping, by =c("check_var_regex"), multiple = "all", relationship = "many-to-many")
      #relationship = "many-to-many"
      
      map %>%
        write.table(file = file.path("skip_logic_map.csv"), sep=",", col.names = T, row.names = F)
      
      # Figure out all the question numbers strings to apply the check on
      questions <- map %>% 
        pull(check_var) %>% 
        unique()
      
      glue::glue("Analysing each column ...")
      pb <- txtProgressBar(min = 0, max = length(questions), style = 3, width = 40)
      k <- 0
      
      # For each such question number ...
      for (q_no in questions) {
        
        # filter the skip logic table for rows that contain condition variable
        skip_filtered_for_q <- map %>% 
          filter(check_var == q_no)
        
        # Apply brackets for each block
        blocks <- skip_filtered_for_q %>% 
          pull(check_var_regex) %>% 
          unique() 
        
        condition_overall <- ""
        
        for (each_block in blocks){
          
          # Skip filtered for block
          skip_filtered_for_block <- skip_filtered_for_q %>% 
            filter(check_var_regex == each_block)
          
          # number of condition variables
          num_conditions <- skip_filtered_for_block %>% 
            nrow()
          
          # initialise condition text
          condition <- ""
          
          # for each condition ...
          for (i in 1:num_conditions){
            
            # get variable on which to apply the check 
            q <- skip_filtered_for_block[i, "check_var"]
            
            # get condition variable 
            var <- skip_filtered_for_block[i, "condition_var"]
            
            if (!is.na(var)){
              
              # get relation between condition variable and the values
              sign <- skip_filtered_for_block[i, "sign"]
              
              # get all allowed values of condition variable, i.e. response vector
              response_vector <- skip_filtered_for_block[i, "response"] %>% 
                strsplit(split = "\\|") %>% 
                gdata::trim() %>% 
                unlist()
              
              # calculate size of this response vector
              num_response <- length(response_vector)
              
              # check if response vector is numeric or charecter
              response_is_string <- response_vector %>% 
                as.numeric() %>% 
                is.na() %>%
                suppressWarnings() %>% 
                sum()
              
              if (num_response > 1){
                if (response_is_string > 0){
                  
                  # if more than one response in string format, create c("a", "b", "c")
                  str <- paste(response_vector, collapse = '", "')
                  response_string <- glue::glue('c("{str}")') 
                } else {
                  
                  # if more than one response in numeric format, create c(1, 3, 5, 9) 
                  str <- paste(response_vector, collapse = ', ')
                  response_string <- glue::glue('c({str})')
                }
              } else {
                
                # if a single response ...
                if (sign == "not in") {sign = "!="}
                if (sign == "in") {sign = "=="}
                if (response_is_string > 0){
                  
                  # ... in string format, create "a"
                  str <- response_vector[1]
                  response_string <- glue::glue('"{str}"')
                } else {
                  
                  # ... in numeric format, create 1
                  str <- response_vector[1]
                  response_string <- glue::glue('{str}')
                }
              }
            }
            
            # get the & / | info before connecting the next condition 
            next_condition <- skip_filtered_for_block[i, "next_condition"] %>% 
              stringr::str_trim()
            
            if (is.na(next_condition)) {next_condition = ""}
            
            # apend to previous condition and make it ready to append the condition string using the "next condition" string
            if (!is.na(var)){
              if (sign == "not in") {
                condition <- glue::glue("{condition} !({var} %in% {response_string}) {next_condition}")
              } else if(sign == "in") {
                condition <- glue::glue("{condition} {var} %in% {response_string} {next_condition}")
              } else {
                condition <- glue::glue("{condition} {var} {sign} {response_string} {next_condition}")
              }
            } else {
              condition <- glue::glue("{condition} {next_condition}")
            }
          }
          
          condition <- condition %>% 
            gdata::trim()
          
          if (condition_overall == ""){
            condition_overall <- glue::glue("({condition})")
          } else {
            condition_overall <- glue::glue("{condition_overall} & ({condition})")
          }
        }
        
        multiple_q = T
        
        apply_condn_on_data <- dt_02 %>% 
          mutate(
            xx_value = ifelse(is.na(eval(parse(text=condition_overall))), F, eval(parse(text=condition_overall))),
            xx_condition = ifelse(xx_value, "met", "un-met"),
            xx_response = case_when(
              is.na(!!rlang::sym(q_no)) ~ "blank",
              !!rlang::sym(q_no) == "" ~ "blank",
              T ~ "value"
            )
          )
        
        # calculate data points for question
        row_count <- apply_condn_on_data %>% 
          nrow()
        
        value_count <- apply_condn_on_data %>% 
          filter(xx_response == "value") %>% 
          nrow()
        
        met_count <- apply_condn_on_data %>% 
          filter(xx_condition == "met") %>% 
          nrow()
        
        error_count <- apply_condn_on_data %>% 
          filter((xx_response == "blank" & xx_condition == "met") |
                   (xx_response == "value" & xx_condition == "un-met")) %>% 
          nrow()
        
        value_when_cond_unmet <- apply_condn_on_data %>% 
          filter(xx_response == "value" & xx_condition == "un-met") %>% 
          nrow()
        
        blank_when_cond_met <- apply_condn_on_data %>% 
          filter(xx_response == "blank" & xx_condition == "met") %>% 
          nrow()
        
        if (error_count > 0) {
          error_id <- apply_condn_on_data %>% 
            filter((xx_response == "blank" & xx_condition == "met") |
                     (xx_response == "value" & xx_condition == "un-met")) %>% 
            select(args[2]) %>% 
            as.list() %>%
            paste(collapse=" | ")
        } else {
          error_id <- ""
        }
        
        skip_logic_log2 <- data.frame(var_to_be_checked = q_no, 
                                      total_rows = row_count, 
                                      num_values = value_count,
                                      rows_that_satisfy_condition = met_count,
                                      num_violations = error_count,
                                      value_when_condition_unmet = value_when_cond_unmet,
                                      blank_when_condition_met = blank_when_cond_met,
                                      condition = condition_overall,
                                      error_id = error_id) 
        
        skip_logic_log <- skip_logic_log %>% 
          rbind(skip_logic_log2)
        
        d_skip <- data.frame(q_no = q_no,
                             condition = condition_overall) %>%
          rbind(d_skip)
        
        k = k + 1
        
        setTxtProgressBar(pb, k)
      }
      
      # Add skip logic for additional columns created
      if (nrow(d_skip_newcol) > 0){
        
        skip_newcol2 <- d_skip_newcol %>%
          mutate(old2 = stringr::str_replace_all(old, "&", "")) %>% 
          mutate(old2 = stringr::str_replace_all(old2, "\\(", "")) %>% 
          mutate(old2 = stringr::str_replace_all(old2, "\\)", "")) %>% 
          mutate(old2 = stringr::str_replace_all(old2, "\\|", "")) %>% 
          mutate(old2 = stringr::str_replace_all(old2, "\\!", "")) %>% 
          mutate(old2 = stringr::str_replace_all(old2, "  ", " ")) 
        
        
        for (row in 1:nrow(skip_newcol2)){
          
          y = skip_newcol2 %>% 
            slice(row) %>% 
            pull(new)
          
          x_all = skip_newcol2 %>%
            slice(row) %>% 
            pull(old2) %>% 
            strsplit(" ")
          x_all <- x_all[[1]]
          
          condition <- skip_newcol2[row, "old"]
          
          for (x in x_all){
            
            condn <- d_skip %>% 
              filter(q_no %in% x) %>% 
              pull(condition)
            
            if (length(condn)==0){
              condn = '("T")'
            } else {
              condn <- condn %>% 
                paste0("(", ., ")")
            }
            
            condition <- condition %>% 
              stringr::str_replace_all(x, condn)
          }
          
          d_skip <- data.frame(q_no = y, condition = condition) %>%
            rbind(d_skip) %>% 
            unique()
        }
      }
      
      
      # OUTPUT LOGS
      skip_logic_log %>%
        select(var_to_be_checked, 
               num_values,
               num_violations,
               value_when_condition_unmet,
               blank_when_condition_met) %>%
        write.table(file = file.path("temp.csv"), sep=",", col.names = F, row.names = F)
      
      skip_logic_log %>%
        select(var_to_be_checked, 
               num_values,
               num_violations,
               value_when_condition_unmet,
               blank_when_condition_met,
               error_id,
               condition) %>%
        write.csv(file.path("skip_logic_error_log.csv"), row.names=FALSE)
    }
    #====================================================
    
    # Log of run ----
    glue::glue("\n") %>% f_log_string(g_file_log)
    glue::glue("finished run in {round(Sys.time() - start_time, 0)} secs. Saving the analysis environment") %>% f_log_string(g_file_log)
    glue::glue("\n") %>% f_log_string(g_file_log)
    
    # clean and save environment in local drive ----
    rm(list = setdiff(ls(), ls(pattern = "^(d_|g_|f_)")))
    save.image(file=file.path(g_wd, "env.RData"))
    
    print(glue::glue("\n\nAll done!"))
    for(i in 1:3){
      print(glue::glue("Finishing in: {4 - i} sec"))
      Sys.sleep(1)
    }
  }, 
  
  warning = function(x){
    msg = glue::glue("{toString(x)}\n\ncheck code '{code_full}'. Issue question: {q_no}. Also check skip_logic_map.csv for NA in the last column.")
    tcltk::tk_messageBox(type = c("ok"), msg, caption = "WARNING!", default = "", icon = "warning")
  },
  
  error = function(x){
    msg = glue::glue("{toString(x)}\n\ncheck code '{code_full}'. Issue question: {q_no}. Also check skip_logic_map.csv for NA in the last column.")
    tcltk::tk_messageBox(type = c("ok"), msg, caption = "ERROR!", default = "", icon = "error")
  }
)




