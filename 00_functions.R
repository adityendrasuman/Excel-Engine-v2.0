# LOAD/INSTALL NEEDED LIBRARIES
f_libraries <- function(necessary.std, necessary.github){
  
  # Define and create user lib path (if it doesnt exist) where they have write permission for sure
  lib_path <- file.path(path.expand('~'), "ExR", "Rlib")
  dir.create(lib_path, recursive = TRUE, showWarnings = FALSE)
  lib_default <- .libPaths(c(""))
  
  # Add user libpath to .libPaths()
  .libPaths(c(lib_path, .libPaths()))
  
  # check list of missing packages in the libpaths
  d1 <- installed.packages()
  d2 <- as.data.frame(d1)
  
  d2$LibPath <- gsub("\\|\\|", "|", gsub("/", "|", gsub("\\\\", "|", d2[,"LibPath"])))
  allowed_path <- gsub("\\|\\|", "|", gsub("/", "|", gsub("\\\\", "|", c(lib_path, lib_default))))
  
  d3 <- d2[d2$LibPath %in% allowed_path,]
  installed_for_interface <- unlist(d3$Package)
    
  missing.std <- necessary.std[!(necessary.std %in% installed_for_interface)]
  missing.github <- necessary.github[!(necessary.github %in% installed_for_interface)]
    
  # install missing packages from standard library
  if(length(missing.std)){
    print(paste("installing standard packages that are not installed: ", missing.std))
    suppressMessages(suppressWarnings(install.packages(missing.std, 
                                                       repos = "http://cran.us.r-project.org",
                                                       lib = lib_path)))
  }
  
  # install missing packages from github
  if(length(missing.github)){
    print(paste("following standard packages currently not installed: ", missing.github))
    print("installing ...")
    
    if("surveytools" %in% missing.github) {
      suppressWarnings(suppressMessages(devtools::install_github("devvartpoddar/surveytools", 
                                                                 ref = "logging",
                                                                 lib = lib_path)))
    }
  }
  
  loaded <- vector()
  missing <- vector()
  # load all the packages into the environment
  for (lib in unique(c(necessary.std, necessary.github))){
    cond <- suppressMessages(suppressWarnings(require(lib, character.only = TRUE,
                                                      quietly = TRUE)))
    loaded <- c(loaded, cond)
    if (cond == F) {missing <- c(missing, lib)}
  }
  
  # throw error if unloaded
  if (!all(loaded)) return(paste0("Following libraries could not be loaded: ", missing))
  if (all(loaded)) return("All libraries loaded successfully")
}

# Apply workaround for openxlsx not loading xlsm file starting version 4.2.4
f_read_xl <- function(path, namedRegion, colNames = F, rowNames = F){
  
  success <- tryCatch(
    {
      openxlsx::read.xlsx(path, namedRegion = namedRegion, colNames = colNames, 
                                rowNames = rowNames)
    },
    error = function(e){
      glue::glue("slowed due to openxlsx library version issue (to be fixed soon) ...") %>% print()
      df <- openxlsx::loadWorkbook(path) %>% 
        openxlsx::read.xlsx(namedRegion = namedRegion, colNames = colNames,  
                              rowNames = rowNames)
      return(df)
    }
  )

  return (success)
}

# Nested progress bars
f_progress <- function(...)    {
  graphics.off()
  vectOfBar <- c(...)*100
  numOfBar <- length(vectOfBar)
  if(.Platform$OS.type == "windows"){windows(width=8, height=numOfBar)}
  par(mar = c(1, 1, 1, 1))
  plot(c(0,100), c(0,numOfBar), type='n', 
       xlab='', ylab='', yaxt='n', xaxt='n', xlim=c(0, 1.05))
  for(i in 1:numOfBar) {
    plotrix::gradient.rect(0, 0.1+i-1, vectOfBar[i], 0.9+i-1, 
                           greens=c(seq(0.2,0.9,length=35),seq(0.9,0.6,length=15)),
                           reds = c(rep(0,25), seq(0,0.9,length=10),seq(0.9,0,length=10), rep(0,5)),
                           blues = c(rep(0,25), seq(0,0.9,length=10),seq(0.9,0,length=10), rep(0,5)),
                           border = NA, gradient="y")
    text(vectOfBar[i]+0.5, 0.5+i-1, paste(round(vectOfBar[i],0), '%', sep=''), adj=0)
  }
}

# ID ANY GIVEN REGEX CHARECTER IN ALL THE COLUMNS OF THE DATA
f_id_char <- function(data, search_chr){
  
  options(dplyr.summarise.inform = FALSE)
  
  summary <- matrix(ncol=2,nrow=0) %>% 
    data.frame() %>% 
    select(var_name = 1, freq = 2)
  
  pb <- txtProgressBar(min = 1, max = ncol(data), style = 3, width = 40)
  
  for (i in 1:ncol(data)) {
    
    col_data <- data[[i]] %>% 
      unlist() %>% 
      table() %>% 
      data.frame()
    
    if (nrow(col_data) > 0) {
      col_data <- col_data %>% 
        rename(var_name = 1, freq = 2)
      if (ncol(col_data)==2)  {
        summary <- col_data %>% 
          filter(stringr::str_detect(var_name, search_chr)) %>% 
          rbind(summary)
      }
    }
    
    utils::setTxtProgressBar(pb, i)
  }
  close(pb)
  
  if (nrow(summary) > 0) {
    summary <- summary %>% 
      dplyr::group_by(var_name) %>% 
      dplyr::summarise(tot_occurance = sum(freq, na.rm = T)) %>%
      data.frame() %>% 
      select(var_name)
    return(summary)
  }
}

f_combine_columns <- function(data, new_column_name, pattern, type = "starts_with") {
  # Capturing the selected column name and selecting names
  quo_column_name <- rlang::enquo(new_column_name)
  selecting_function <- switch(
    type,
    "starts_with" = starts_with,
    "ends_with" = ends_with,
    "matches" = matches,
    "contains" = contains,
    "one_of" = one_of,
    starts_with
  )
  
  data %>%
    dplyr::mutate_at(vars(selecting_function(pattern)), trimws) %>%
    dplyr::mutate_at(vars(selecting_function(pattern)), ~ ifelse(is.na(.), "", .)) %>%
    tidyr::unite(!!quo_column_name,
                 selecting_function(pattern),
                 sep = "|",
                 remove = F) %>%
    dplyr::mutate(!!quo_column_name := gsub("\\|+", "|", !!quo_column_name),
           !!quo_column_name := trimws(!!quo_column_name),
           !!quo_column_name := gsub("\\|$", "", !!quo_column_name),
           !!quo_column_name := stringr::str_remove_all(!!quo_column_name, "^\\|"),
           !!quo_column_name := stringr::str_remove_all(!!quo_column_name, "^\\| ")
    )
}

f_cross_tab <- function(.data, 
                      row_var, 
                      col_var, 
                      value, 
                      ...,
                      combine = "sgnf") {
  
  # Capture all variables as symbols
  eq_row <- rlang::enquo(row_var)
  eq_col <- rlang::enquo(col_var)
  eq_val <- rlang::enquo(value)
  eq_com <- rlang::expr(c(...))
  tmp    <- rlang::sym(combine)
  
  # keep only the variables that are present as row, col, value or eq
  .data <- .data %>% 
    mutate(sgnf = case_when(
      sgnf %in% c("-----", "<<--|") ~ "",
      TRUE ~ sgnf
    )) %>%
    select(!!eq_row, !!eq_col, !!eq_val, !!eq_com, !!tmp) %>%
    tidyr::unite(`_____TEMP VAL COL_____`, !!tmp, !!eq_val, sep = " ", na.rm = TRUE) %>%
    dplyr::mutate(`_____TEMP VAL COL_____` = trimws(`_____TEMP VAL COL_____`))
  
  
  # spread the col variable out, and keep the row
  .data <- .data %>%
    tidyr::spread(!!eq_col, `_____TEMP VAL COL_____`, drop = TRUE) 
  
  as_tibble(.data)
  
}

f_answer_creator <- function(data, s, y, condition_2 = "T", ...){
  
  if (s %in% colnames(d_summ) & !is.na(y) & y != ""){  
    
    id_cols = c(d_summ["ID", s])
    strata_cols = c(d_summ["STRATA", s]) %>% 
      strsplit("\\|")
    strata_cols = strata_cols[[1]] %>% 
      stringr::str_trim()
    weight_col = d_summ["WEIGHT", s]
    
    summariser <- purrr::partial(summariser_base, survey.design = list(
      id = id_cols,
      strata = strata_cols,
      weight = weight_col,
      nest = TRUE))
    
    y_sym <- y %>% rlang::sym()
    x_all <<- c(...)
    
    if (class(data[[y]]) != "numeric"){
      
      data[[y_sym]] <- as.character(data[[y_sym]])
      
      if (length(x_all) == 0){
        
        summary <- data %>%
          mutate(value_2 = eval(parse(text=condition_2))) %>%
          filter(value_2 == T) %>%
          summariser(!!y_sym)
        
      } else if (length(x_all) > 0){
        
        x_all_regex <- vector()
        for (x in x_all){x_all_regex <- c(x_all_regex, paste0("^", x, "$"))}
        
        summary <- data %>%
          f_combine_columns(group_var_col, x_all_regex, "matches") %>%  
          mutate(value_2 = eval(parse(text=condition_2))) %>% 
          filter(value_2 == T) %>%
          summariser(!!y_sym, group_var_col)
      }
    
    } else if (class(data[[y]]) == "numeric") {
      
      if (length(x_all) == 0){
        
        summary <- data %>%
          mutate(value_2 = eval(parse(text=condition_2))) %>%
          filter(as.logical(value_2) == T) %>%
          summariser(!!y_sym)
        
      } else if (length(x_all) > 0){
        
        x_all_regex <- vector()
        for (x in x_all){x_all_regex <- c(x_all_regex, paste0("^", x, "$"))}
        
        summary <- data %>%
          f_combine_columns(group_var_col, x_all_regex, "matches") %>%  
          mutate(value_2 = eval(parse(text=condition_2))) %>% 
          filter(value_2 == T) %>%
          summariser(!!y_sym, group_var_col) %>% 
          select(-group, group = response)
      }
    }
    
    return(summary)
  }
}

f_pad_lines <- function(str, n){
  lines <- str %>% 
    regmatches(gregexpr("\n", str)) %>%
    lengths() + 1
  
  line_diff <- n - lines
  
  if (line_diff >= 0){
    str <- paste0(str, paste(replicate(line_diff, "\n"), collapse = ""))
  } else {
    for (i in 1:line_diff*-1){
      if (i %% 2 == 0){
        str <- str %>% 
          stringr::str_replace("\n", "")
      }
    }
  }
  return(str)
}

f_graph_1 <- function(.answer, x_all, x_label = "", y_label = "", condition = "", numeric_y){
  
  options(dplyr.summarise.inform = FALSE)
  
  width_x_label = 10
  width_facet_label = 20
  font_value_label = 8
  font_legend_label = 8
  font_facet_label = 8
  font_x_label = 8
  font_caption = 8
  max_caption_lines = 7
  
  if (condition != ""){
    condition <- condition %>%
      stringr::str_replace_all("//T //& ", "") %>% 
      stringr::str_replace_all('"T"', "Everyone") %>% 
      stringr::str_replace_all("&", "\nAND,") %>% 
      stringr::str_replace_all("%in%", "in") %>% 
      stringr::str_replace_all("==", "=")
    
    condition <- condition %>% 
      f_pad_lines(max_caption_lines - 2)
    
    condition <- glue::glue("Respondent pool:\n------------------------\n {condition}")
  }
  
  len <- length(x_all)
  
  if (len == 0){
    x_top <- "group" %>% 
      rlang::sym()
    
    x_top_label <- ""
  }
  
  if (len >= 1){
    x_top <- x_all[1] %>% 
      rlang::sym()
    
    x_top_label <- x_label[1]
  }
  
  if (len >= 2){
    x_second <- x_all[2] %>% 
      rlang::sym()
  }
  
  if (len >= 3){
    x_last <- x_all[len] %>% 
      rlang::sym()
  }
  
  if (len >= 4){
    x_subsequent <- x_all[3:len - 1] %>% 
      rlang::syms()
  }
  
  n_size <- .answer %>%
    filter(if (len >= 1) group != "Overall" else T) %>% 
    dplyr::group_by(group) %>% 
    dplyr::summarise(n = sum(N)) %>% 
    mutate(n = paste0("(n = ", scales::comma(n, accuracy = 1), ")"))
  
  cols_response <- c(response = y_label)
  
  .answer <- .answer %>% 
    tibble::add_column(!!!cols_response[!names(cols_response) %in% names(.)])
  
  .data <- .answer %>% 
    filter(if (len >= 1) group != "Overall" else T) %>%
    select(group, response, value, value_low, value_upp) %>%
    dplyr::mutate(value = as.numeric(gsub("%", "", value)),
                  value_low = as.numeric(gsub("%", "", value_low)),
                  value_upp = as.numeric(gsub("%", "", value_upp))) %>%
    left_join(n_size, by = "group")
  
  num_x_labels <- nrow(.data)/length(unique(.answer$response))
  
  if (len > 0){
    .data <- .data %>% 
      tidyr::separate(group, x_all, sep = "\\|")
  }
  
  if (numeric_y == T){
    .data <- .data %>% 
      dplyr::arrange(desc(value))
  }
  
  .data <- .data %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(!!x_top := f_pad_lines(paste0(stringr::str_wrap(!!x_top, width = width_x_label), "\n", n), 4)) %>% 
    mutate(response = forcats::fct_inorder(response)) %>% 
    mutate(!!x_top := forcats::fct_inorder(!!x_top))
  
  ang <- ifelse(num_x_labels > 15, 90, 0) 
  
  # ASSIGN X AXIS
  if (len == 0){
    p <- .data %>%
      ggplot2::ggplot(ggplot2::aes(x = group, y=value, fill = response))
  } else if (len >= 1){
    p <- .data %>%
      ggplot2::ggplot(ggplot2::aes(x=!!x_top, y=value, fill = response))
  }
  
  # ASSIGN AXIS LABELS
  if (numeric_y == F){
    p <- p + 
      ggplot2::ylab("% Responses") + 
      ggplot2::xlab(x_top_label)
  } else {
    y_label <- .answer %>% 
      slice(1) %>% 
      pull(response)
    
    p <- p + 
      ggplot2::ylab(y_label) + 
      ggplot2::xlab(x_top_label)
  }
  
  # ASSIGN FACET GRIDS
  if (len == 2){
    p <- p + ggplot2::facet_grid(as.formula(paste0(". ~ ", x_second)), 
                                 labeller = label_wrap_gen(width = width_facet_label, multi_line = TRUE),
                                 drop = T,
                                 scale = "free_x")
    
  } else if (len == 3) {
    p <- p + ggplot2::facet_grid(as.formula(
      paste0(".", " ~ ", x_second, " + ", x_last)
    ), 
    labeller = label_wrap_gen(width = width_facet_label, multi_line = TRUE),
    drop = T,
    scale = "free_x")
    
  }  else if (len >= 4) {
    p <- p + ggplot2::facet_grid(as.formula(
      paste0(".", " ~ ", x_second, " + ", paste(x_subsequent, collapse = " + "), " + ", x_last)
    ), 
    labeller = label_wrap_gen(width = width_facet_label, multi_line = TRUE),
    drop = T,
    scale = "free_x")
  }
  
  # CREATE FINAL GRAPH
  p <- p +
    ggplot2::ggtitle(toupper(y_label)) +
    ggplot2::labs(caption = condition) +
    ggplot2::geom_bar(position=ggplot2::position_dodge(0.95), stat="identity") + 
    ggplot2::geom_errorbar(ggplot2::aes(ymax=value_upp, ymin=value_low), 
                           position = ggplot2::position_dodge(0.95), 
                           width = 0.2, size=.5, color="dark red") +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.1))) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(vjust = 0.5, size=font_x_label, angle = ang),
      axis.ticks.x = ggplot2::element_line(colour = "grey"),
      axis.line.x = ggplot2::element_line(colour = "grey"),
      
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.line.y = ggplot2::element_blank(),
      
      legend.position = ifelse(numeric_y==T, "none", "top"),
      legend.justification = ifelse(numeric_y==T, "none", "left"),
      legend.title = ggplot2::element_blank(),
      legend.box = "horizontal",
      legend.text=ggplot2::element_text(size=font_legend_label),
      legend.key.size = ggplot2::unit(0.3, "cm"),
      
      panel.grid.major = ggplot2::element_blank(), 
      panel.grid.minor = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      panel.spacing = ggplot2::unit(2, "lines"),
      
      strip.placement = "outside",
      strip.text.x = ggplot2::element_text(size = font_facet_label, colour = "black"),
      
      plot.caption = ggplot2::element_text(colour = "dark grey", size=font_caption))
  
  # DIFFERENCE FOR NUMERIC VS CATEGORICAL
  if (numeric_y == T){
    p <- p + 
      ggplot2::geom_text(ggplot2::aes(label=value), 
                         position=ggplot2::position_dodge(width=1), 
                         vjust=-1,
                         size=3)
  } else {
    p <- p + 
      ggplot2::geom_text(ggplot2::aes(label=scales::number(value, accuracy = 1.1)), 
                         position=ggplot2::position_dodge(width=1), 
                         vjust=-1,
                         size=3)
  }
  return(p)
}

f_graph_2 <- function(.answer, 
                      x_all,
                      y,
                      y_condition = "T", 
                      condition = "", 
                      numeric_y, 
                      colmap,
                      cluster_chart = T){
  
  options(dplyr.summarise.inform = FALSE)
  
  width_x_label = 10
  width_facet_label = 20
  font_value_label = 8
  font_legend_label = 8
  font_facet_label = 8
  font_x_label = 8
  font_caption = 8
  max_caption_lines = 7
  
  if (condition != ""){
    condition <- condition %>%
      stringr::str_replace_all("//T //& ", "") %>% 
      stringr::str_replace_all('"T"', "Everyone") %>% 
      stringr::str_replace_all("&", "\nAND,") %>% 
      stringr::str_replace_all("%in%", "in") %>% 
      stringr::str_replace_all("==", "=") %>% 
      trimws()
    
    if (condition == "T") {condition <- "Everyone"}
    
    y_condition2 <- y_condition %>%
      stringr::str_replace_all("//T //& ", "") %>% 
      stringr::str_replace_all('"T"', "Everyone") %>% 
      stringr::str_replace_all("&", "\nAND,") %>% 
      stringr::str_replace_all("%in%", "in") %>% 
      stringr::str_replace_all("==", "=") %>% 
      trimws()
    
    if (y_condition2 == "T") {y_condition2 <- "All responses"}
    
    y_condition2 <- y_condition2 %>%  
      f_pad_lines(max_caption_lines - 2)
    
    condition <- glue::glue("------------------------\nRESPONDENT POOL: {condition}\n------------------------\nSHOW: {y_condition2}")
  }
  
  questions <- .answer %>% 
    pull(question) %>% 
    unique()
  
  count_q <- questions %>% 
    length()
  
  if (count_q > 1) {
    y_label <- questions %>% 
      gsub("\\|:.*$", "", .) %>% 
      trimws() %>% 
      unique()
    
    y_label <- y_label[1]
    x_new <- c("question", x_all)
    
  } else {
    y_label <- questions[1]
    x_new <- x_all
  }
  
  len <- length(x_new)
  
  if (len == 0){
    x_top <- "group" %>% 
      rlang::sym()
    
    x_top_label <- ""
  }
  
  if (len >= 1){
    x_top <- x_new[1] %>% 
      rlang::sym()
    
    if (count_q > 1) {
      x_top_label <- y_label
    } else {
      x_top_label <- colmap %>% 
        filter(X1 == x_top) %>% 
        pull(X2)
    }
  }
  
  if (len >= 2){
    x_second <- x_new[2] %>% 
      rlang::sym()
    
    x_second_label <- colmap %>% 
      filter(X1 == x_second) %>% 
      pull(X2)
  }
  
  if (len >= 3){
    x_last <- x_new[len] %>% 
      rlang::sym()
    
    x_last_label <- colmap %>% 
      filter(X1 == x_last) %>% 
      pull(X2)
  }
  
  if (len >= 4){
    x_subsequent <- x_new[3:len - 1] %>% 
      rlang::syms()
    
    x_subsequent_label <- colmap %>% 
      filter(X1 == x_subsequent) %>% 
      pull(X2)
  }
  
  # If response column is not present, add it with value as question string | Replace question with difference
  
  if (numeric_y == T) {
    cols_response <- c(response = y_label)
    .answer <- .answer %>% 
      #select(-response) %>% 
      tibble::add_column(!!!cols_response[!names(cols_response) %in% names(.)]) %>% 
      mutate(question = trimws(gsub("^.*\\|:", "", .$question)))
  } else {
    .answer <- .answer %>% 
      mutate(question = trimws(gsub("^.*\\|:", "", .$question)))
  }

  n_size <- .answer %>%
    filter(if (len >= 1) group != "Overall" else T) %>% 
    dplyr::group_by(group, question) %>% 
    dplyr::summarise(n = sum(N)) %>% 
    mutate(n = paste0("(n = ", scales::comma(n, accuracy = 1), ")"))
  
  .data <- .answer %>% 
    filter(if (len >= 1) group != "Overall" else T) %>%
    select(group, question, response, value, value_low, value_upp) %>%
    dplyr::mutate(value = as.numeric(gsub("%", "", value)),
                  value_low = as.numeric(gsub("%", "", value_low)),
                  value_upp = as.numeric(gsub("%", "", value_upp))) %>%
    left_join(n_size, by = c("group", "question")) %>% 
    filter(ifelse(is.na(eval(parse(text=y_condition))), F, eval(parse(text=y_condition))))
  
  num_x_labels <- nrow(.data)/length(unique(.data$response))
  
  if (count_q > 1){
    if (len >= 2){
      .data <- .data %>% 
        tidyr::separate(group, x_all, sep = "\\|")
    }
  } else {
    if (len >= 1){
      .data <- .data %>% 
        tidyr::separate(group, x_all, sep = "\\|")
    }
  }

  if (numeric_y == T){
    .data <- .data %>% 
      dplyr::arrange(desc(value))
  }
  
  .data <- .data %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(!!x_top := f_pad_lines(paste0(stringr::str_wrap(!!x_top, width = width_x_label), "\n", n), 4)) %>% 
    mutate(response = forcats::fct_inorder(response)) %>% 
    mutate(!!x_top := forcats::fct_inorder(!!x_top))
  
  ang <- ifelse(num_x_labels > 15, 90, 0) 
  
  # ASSIGN X AXIS
  if (count_q > 1){
    if (len == 1){
      p <- .data %>%
        ggplot2::ggplot(ggplot2::aes(x = group, y=value, fill = response))
    } else if (len >= 2){
      p <- .data %>%
        ggplot2::ggplot(ggplot2::aes(x=!!x_top, y=value, fill = response))
    }
  } else {
    if (len == 0){
      p <- .data %>%
        ggplot2::ggplot(ggplot2::aes(x = group, y=value, fill = response))
    } else if (len >= 1){
      p <- .data %>%
        ggplot2::ggplot(ggplot2::aes(x=!!x_top, y=value, fill = response))
    }
  }
  
  # ASSIGN AXIS LABELS
  if (numeric_y == F){
    p <- p + 
      ggplot2::ylab("% Responses") + 
      ggplot2::xlab(x_top_label)
  } else {
    p <- p + 
      ggplot2::ylab("Value") + 
      ggplot2::xlab(x_top_label)
  }
  
  # ASSIGN FACET GRIDS
  if (len == 2){
    p <- p + ggplot2::facet_grid(as.formula(paste0(". ~ ", x_second)), 
                                labeller = label_wrap_gen(width = width_facet_label, multi_line = TRUE),
                                drop = T,
                                scale = "free_x")
    
  } else if (len == 3) {
    p <- p + ggplot2::facet_grid(as.formula(paste0(".", " ~ ", x_second, " + ", x_last)), 
                                labeller = label_wrap_gen(width = width_facet_label, multi_line = TRUE),
                                drop = T,
                                scale = "free_x")
    
  }  else if (len >= 4) {
    p <- p + ggplot2::facet_grid(as.formula(paste0(".", " ~ ", x_second, " + ", paste(x_subsequent, collapse = " + "), " + ", x_last)), 
                                labeller = label_wrap_gen(width = width_facet_label, multi_line = TRUE),
                                drop = T,
                                scale = "free_x")
  }
  
  # CREATE FINAL GRAPH
  p <- p +
    ggplot2::ggtitle(toupper(glue::glue(paste(strwrap(paste0(y, ": ", y_label), width = 150), collapse = "\n")))) +
    ggplot2::labs(caption = condition)
  
  
  
  if (cluster_chart == T){
    p <- p +
      ggplot2::geom_bar(position=ggplot2::position_dodge(0.95), stat="identity") + 
      ggplot2::geom_errorbar(ggplot2::aes(ymax=value_upp, ymin=value_low), 
                             position = ggplot2::position_dodge(0.95), 
                             width = 0.2, size=.5, color="dark red")
  
  } else if (cluster_chart == F){
    p <- p +
      ggplot2::geom_bar(position=ggplot2::position_stack(0), stat="identity")
  }
  
  p <- p +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.1))) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(vjust = 0.5, size=font_x_label, angle = ang),
      axis.ticks.x = ggplot2::element_line(colour = "grey"),
      axis.line.x = ggplot2::element_line(colour = "grey"),
      
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.line.y = ggplot2::element_blank(),
      
      legend.position = ifelse(numeric_y==T, "none", "top"),
      legend.justification = ifelse(numeric_y==T, "none", "left"),
      legend.title = ggplot2::element_blank(),
      legend.box = "horizontal",
      legend.text=ggplot2::element_text(size=font_legend_label),
      legend.key.size = ggplot2::unit(0.3, "cm"),
      
      panel.grid.major = ggplot2::element_blank(), 
      panel.grid.minor = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      panel.spacing = ggplot2::unit(2, "lines"),
      
      strip.placement = "outside",
      strip.text.x = ggplot2::element_text(size = font_facet_label, colour = "black"),
      
      plot.caption = ggplot2::element_text(colour = "dark grey", size=font_caption))
  
  # DIFFERENCE FOR NUMERIC VS CATEGORICAL
  if (numeric_y == T){
    p <- p + 
      ggplot2::geom_text(ggplot2::aes(label=value), 
                         position=ggplot2::position_dodge(width=1), 
                         vjust=-1,
                         size=3)
  } else {
    p <- p + 
      ggplot2::geom_text(ggplot2::aes(label=scales::number(value, accuracy = 1.1)), 
                         position=ggplot2::position_dodge(width=1), 
                         vjust=-1,
                         size=3)
  }
  return(p)
}

f_graph_error1 <- function(q_no){
  
  # ASSIGN X AXIS
  p <- ggplot2::ggplot(data.frame(), ggplot2::aes(x = "", y="", fill = "")) +
    ggplot2::ylab(q_no) + 
    ggplot2::xlab("ERROR") +
    ggplot2::ggtitle(glue::glue("ERROR: At card number {q_no} of the selected card set"))
  
  return(p)
}

f_graph_section <- function(section_name){
  
  # ASSIGN X AXIS
  p <- ggplot2::ggplot(data.frame(), ggplot2::aes()) +
    ggplot2::ggtitle(glue::glue(paste(strwrap(paste0("\n\n",section_name), width = 50), collapse = "\n"))) + 
    ggplot2::theme(plot.title = element_text(size = 40, face = "bold")) +
    ggplot2::theme(plot.title = element_text(hjust = 0.5)) + 
    ggplot2::theme(panel.background = element_blank())
  
  return(p)
}

f_plotter <- function(graph, location){
  
  fn = file.path(location, 
                 paste0("Latest plots ", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf"))
  
  graph <- graph[lengths(graph) != 0]
  
  ggplot2::ggsave(
    plot = gridExtra::marrangeGrob(graph, nrow=1, ncol=1), 
    filename=fn,
    width=16,
    height=6
  )
  
  shell.exec(fn)
}


f_log_string <- function(str, output){
  
  str = glue::glue(str)
  print(str)
  cat(str, file=output, sep="\n", append=TRUE)

}

f_log_table <- function(df, title="", output){
  
  if (is.null(nrow(df))) {
    cat(paste0("   | ", trimws(paste0("TITLE: ", title))), file = output, 
        append = TRUE, sep = "\n")
    cat(paste0("   ", "Table has zero rows. Can not be written"), file = output, 
        append = TRUE, sep = "\n")
    cat("", file = output, append = TRUE, sep = "\n")
  
  } else {
    
    if (nrow(df) == 1) { df[nrow(df) + 1, ] <- NA } 
    
    df <- cbind("   " = "   ", df)
    
    tot <- 0
    cnames <- colnames(df)
    separator <- colnames(df)
    n <- as.matrix(nchar(cnames))
    
    d <- apply(df, 2, format)
    n <- apply(cbind(n, nchar(d[1,])), 1, max)
    
    fmts <- paste0("%",n, "s")
    for (i in 1:length(cnames)) {
      cnames[i] <- sprintf(fmts[i], cnames[i])
      separator[i] <- paste(replicate(n[i], "-"), collapse = "")
      d[,i] <- sprintf(fmts[i], trimws(d[,i]))
      tot <- tot + n[i] + 1
    }
    d <- rbind(cnames, separator, d)
    
    cat("", file = output, append = TRUE, sep = "\n")
    cat(paste0("   ", paste(replicate(ceiling(tot) - 3, "#"), collapse = "")), file = output, append = TRUE, sep = "\n")
    cat(paste0("   | ", trimws(gsub("_", " ", paste0("TITLE: ", title)))), file = output, append = TRUE, sep = "\n")
    cat(paste0("   ", paste(replicate(ceiling(tot) - 3, "="), collapse = "")), file = output, append = TRUE, sep = "\n")
    write.table(d, output, quote = F, append = T, row.names = F, col.names = F, na = "<NA>", sep = "|")
    cat(paste0("   ", paste(replicate(ceiling(tot) - 3, "="), collapse = "")), file = output, append = TRUE, sep = "\n")
    cat(paste0("   ", paste(replicate(ceiling(tot) - 3, "#"), collapse = "")), file = output, append = TRUE, sep = "\n")
    cat("", file = output, append = TRUE, sep = "\n")
  }
}

f_grouper <- function(data){
  data %>% 
    group_by_all() %>% 
    count() %>% 
    as.data.frame()
}

#___________________----

f_segmentor <- function(df_in, 
                        s,
                        y_in,
                        filter_in,
                        x_all_in,
                        file_nm,
                        min_samp,
                        ignore_weight_responses = c(),
                        with_weight = F){
  
  # get weight col from summariser
  weight_col <- d_summ["WEIGHT", s]
  
  # processing Y
  y <- rlang::sym(y_in)
  
  # Apply user provided filter + skip logic filter on Y
  df_1 <- df_in %>% 
    filter(ifelse(is.na(eval(parse(text=filter_in))), F, eval(parse(text=filter_in))))
  
  glue::glue("{nrow(df_1)} rows remaining after skip-logic and user filters") %>% print()
  
  # loop all x and remove x that has just one valid value
  x_all <- c()
  
  for (x in x_all_in){
    x_values <- df_1 %>% 
      select(all_of(x)) %>% 
      unique() %>% 
      filter(!is.na(!!rlang::sym(x))) %>% 
      nrow()
    
    if (x_values > 1){
      x_all <- x_all %>% 
        c(x)
      if (class(df_1[[x]]) != "numeric"){
        df_1[[x]] <- ifelse(is.na(df_1[[x]]), "NA", df_1[[x]])
      }
      if (class(df_1[[x]]) == "character"){
        df_1[[x]] <- forcats::as_factor(df_1[[x]])
      }
      
    } else {
      glue::glue("variable {x} removed as it has only one valid value") %>% print()
    }
  }
  
  # Select relevant variables and remove NA in any variables
  df <- df_1 %>% 
    select(all_of(y), all_of(x_all), all_of(weight_col)) %>% 
    tidyr::drop_na()
  glue::glue("{nrow(df_1) - nrow(df)} rows removed due to NA values") %>% print()

  # TREE ALGO
  min_split_size <- as.numeric(min_samp)
  
  train_control <- caret::trainControl(
    method = "repeatedcv",
    number = 10,
    repeats = 3
  )
  
  # ** create forced data for tree 
  if (with_weight == T) {
    temp <- janitor::tabyl(df, !!y) %>%
      filter(!(!!y %in% ignore_weight_responses))
    
    weighted_class <- temp %>%
      filter(n != max(n)) %>%
      pull(!!y)
    
    if (length(weighted_class) > 0) {
      df <- purrr::map_dfr(weighted_class, function(x) {
        temp_n <- temp %>%
          filter(!!y %in% x) %>%
          pull(n)
        
        df %>%
          filter(!!y %in% x) %>%
          sample_n(size = max(temp$n) - temp_n, replace = TRUE)
      }) %>%
        rbind(df)
    }
  }
  
  df %>% 
    select(all_of(y)) %>% 
    f_grouper() %>% 
    print()
  
  # ** fit a tree
  colnames_for_data <- df %>%
    select(-all_of(weight_col), - !!y) %>%
    colnames() %>%
    paste0(collapse = "+")
  
  tree_formulae <- as.formula(paste(quo_name(y), "~", colnames_for_data)) 
  rm(colnames_for_data)
  
  tree_fit <- caret::train(tree_formulae,
                           data = df,
                           weights = get(weight_col),
                           method = "rpart",
                           parms = list(split = "information"),
                           trControl = train_control,
                           control = rpart::rpart.control(minsplit = min_split_size),
                           tuneLength = 10)
  
  # ** output the chart / message of failure
  if (nrow(tree_fit$finalModel$frame) == 1) {
    glue::glue("The model predicts only a single class and cannot be graphed") %>% print()
    
  } else {
    nm <- paste0(file_nm, ifelse(with_weight == T, " - FORCED", " - ORGANIC")) 
    nm <- gsub("\\:", " \\- ", nm)
    
    png(file.path("..", paste0(nm, ".png")),
        height = 3000,
        width = 8000,
        res = 600)
    
    rpart.plot::rpart.plot(tree_fit$finalModel,
                           main = nm,
                           type = 2,
                           tweak = 1.1,
                           space = 0.1,
                           gap = 0.01)
    dev.off()
    graphics.off()
  }
}


#___________________----

#' Summarise survey data
#'
#' This function provides a summary of survey data based on the design provided,
#' as well as checks for significance of results
#'
#' @param .data Dataframe or survey data to be summarised
#' @param var Variable within dataframe to be summarised
#' @param ... Variables to group by
#' @param stat One of "mean", "median" or "total". Defaults to mean if not correctly provided 
#' @param compare Stat to use for significance tests of survey results. Must be provided in "group::response" format. Defaults to the overall value
#' @param survey.design List of survey design inputs to be applied to dataframe, if not survey data of class 'tbl_svy'. Please see '?survey::svydesign' to identify required inputs
#' @param ci_level confidence level to be summarised for
#' @param simplify Should the survey results be simplified from decimals to numbers?
#' 
#' @return A dataframe of summarised results
#' 
#' @importFrom magrittr "%>%"
#' 
#' @export
summariser_base <- function(.data,
                            var,
                            ...,
                            stat = c("mean", "median", "total"),
                            compare = "overall",
                            survey.design = NULL,
                            ci_level = 0.95,
                            simplify = TRUE) {
  
  # Running method summariser_base
  UseMethod("summariser_base")
}

#' @rdname summariser_base
#' @export
summariser_base.default <- function(.data,
                                    var,
                                    ...,
                                    # allowed stats to ask for
                                    stat = c("mean", "median", "total"),
                                    # metric to compare significance against
                                    compare = "overall",
                                    # Optional list of survey design inputs
                                    survey.design = NULL,
                                    # CI level to be used
                                    ci_level = 0.95,
                                    # Simplify results by converting to percentage
                                    simplify = TRUE) {
  
  # List of checks for inputs ----
  # 1 | Data or the variable are not missing
  if (missing(.data) | missing(var))
    rlang::abort("Please provide a valid data or variable input to summarise")
  
  # Enquoting selected variable
  enquo_var <- rlang::enquo(var)
  
  # 2 | Check if variable is present in data
  tmp <- try(tidyselect::eval_select(enquo_var,
                                     data = tibble::as_tibble(.data)),
             silent = TRUE)
  if (class(tmp) == "try-error")
    rlang::abort(glue::glue("Column {rlang::quo_text(enquo_var)} does not exist in the dataframe"))
  
  enquo_var <- names(tmp)
  rm(tmp)
  
  # Quoting grouping variables
  grouped_vars <- rlang::expr(c(...))
  
  # 3 | Check if group variables are present in data
  tmp <- try(tidyselect::eval_select(grouped_vars,
                                     data = .data),
             silent = TRUE)
  if (class(tmp) == "try-error")
    rlang::abort(glue::glue("Grouping vars provided do not exist in the dataframe. Please check again"))
  
  grouped_vars <- names(tmp)
  rm(tmp)
  
  # 4 | Check if argument for stat matches default values
  stat <- match.arg(stat)
  
  # 5 | Check if compare is a string
  if (!purrr::is_character(compare))
    rlang::abort("`compare` can only take a string of the form {group}::{response} in order to estimate significance")
  
  # 6 | Set up survey if design is not null
  if (!is.null(survey.design)) {
    
    # 6a | Check if variables provided in survey.design match required inputs
    argsList <- c("ids", "probs", "strata", "fpc", 
                  "weights", "nest", "check.strata")
    if (all(names(survey.design) %in% argsList))
      rlang::abort("List of inputs provided in survey design do not match inputs. Please see '?survey::svydesign' to identify required inputs")
    
    rm(argsList)
    
    # 6b | Check if variables provided in survey.design are present in the dataframe
    # TODO
    
    # Set up .data based on survey design
    .data <- do.call(srvyr::as_survey,
                     c(list(.data),
                       survey.design))
    
  }
  
  # 7 | Check if data inherits class accepted by srvyr data
  if (!inherits(.data, "tbl_svy"))
    rlang::abort("Data is not defined as a survey data of class 'tbl_svy'. Please provide a survey.design, or data set up as a survey")
  
  # 8 | Checks for significance numbers and values
  # 8a | Is the CI level numeric
  if (!is.numeric(ci_level)) {
    rlang::warn("Please provide a numeric value for ci_level. Defaulting to 0.95")
    
    ci_level <- 0.95
  }
  
  # 8b | Is the CI level less than or equal to one
  if (any(ci_level > 1)) {
    rlang::warn("CI levels is greater than 1. Defaulting to 0.95")
    
    ci_level <- 0.95
  }
  
  # 8c | Has only one CI level been provided
  if (length(ci_level) > 1) {
    rlang::warn("More than one CI levels are provided. Defaulting to first value")
    
    ci_level <- ci_level[1]
  }
  
  # Define local variables ----
  # 1 | Is the variable numeric or logical
  IS_NUMERIC <- .data %>%
    dplyr::pull(enquo_var) %>%
    inherits(c("integer", "numeric", "logical"))
  
  # Summarising variables ----
  if (IS_NUMERIC) {
    .data <- summariser_numeric(.data,
                                enquo_var,
                                grouped_vars,
                                stat,
                                ci_level)
  } else {
    .data <- summariser_character(.data,
                                  enquo_var,
                                  grouped_vars,
                                  stat,
                                  ci_level)
  }
  
  # Test significance ----
  .data <- test_significance(.data, compare)
  
  # Simplify ----
  if (simplify){
    # 1 | if is character, multiply by 100
    if (!IS_NUMERIC &
        stat != "total") {
      .data <- .data %>%
        dplyr::mutate(dplyr::across(starts_with("value"), ~ scales::percent(.x, digits = 2)))
    } else {
      .data <- .data %>%
        dplyr::mutate(dplyr::across(where(is.numeric), ~round(.x, 2)))
    }
  }
  
  # Add a class of svyt_df to the output
  class(.data) <- c("svyt_df", class(.data))
  
  # Return values ----
  return(.data)
}

#' @importFrom magrittr "%>%"
summariser_numeric <- function(.data, var, group, stat, level) {
  # Summarises variables if they are numeric or logical in nature
  
  # Check if the variable is logical
  lgl <- .data %>%
    dplyr::pull(var) %>%
    inherits(c("logical"))
  
  if (lgl) {
    # If logical, convert it to numeric and print out a message
    .data <- .data %>%
      dplyr::mutate(!!rlang::sym(var) := !!rlang::sym(var) * 1)
    
    rlang::inform(glue::glue("Variable {var} has been converted from logical to numeric. Please force as
                       character if you wish to keep both categories"))
  }
  
  # creating summary function
  sum_fnc <- switch (stat,
                     "mean"   = srvyr::survey_mean,
                     "median" = srvyr::survey_median,
                     "total"  = srvyr::survey_total,
                     srvyr::survey_mean
  )
  
  # Ungrouped summary
  summary_data <- .data %>%
    srvyr::summarise(value = sum_fnc(!!rlang::sym(var),
                                     vartype = c("se", "ci"),
                                     na.rm = TRUE,
                                     level = level,
                                     # If the variable is converted from logical, yes else no
                                     proportion = lgl),
                     N = srvyr::unweighted(dplyr::n())) %>%
    srvyr::mutate(group = "Overall",
                  response = "Overall") %>%
    srvyr::select(group, response, N, value, everything()) %>% 
    dplyr::arrange(-value)
  
  # Grouped summary
  if (length(group) > 0) {
    # If a group is present, do a grouped summary
    grouped_data <- purrr::map_dfr(group, function(group_var) {
      .data %>%
        srvyr::group_by(!!rlang::sym(group_var), .add = FALSE) %>%
        srvyr::summarise(value = sum_fnc(!!rlang::sym(var),
                                         vartype = c("se", "ci"),
                                         na.rm = TRUE,
                                         level = level,
                                         # If the variable is converted from logical, yes else no
                                         proportion = lgl),
                         N = srvyr::unweighted(dplyr::n())) %>%
        srvyr::mutate(group = group_var,
                      response = !!rlang::sym(group_var)) %>%
        srvyr::select(group, response, N, value, everything(), -!!rlang::sym(group_var))
    }
    )
    
    summary_data <- dplyr::bind_rows(summary_data,
                                     grouped_data)
  }
  
  return(summary_data)
}

#' @importFrom magrittr "%>%"
summariser_character <- function(.data, var, group, stat, level) {
  # Summarises variable if they are character or factor in nature
  
  # creating summary function
  sum_fnc <- switch (stat,
                     "mean"  = srvyr::survey_mean,
                     # There is no median for characters or factors. Defaulting to mean
                     "total" = srvyr::survey_total,
                     srvyr::survey_mean
  )
  
  # If stat is median, inform saying that it is not gonna happen
  if (stat == "median")
    rlang::warn("`stat` median is not allowed with character vectors, providing survey mean instead")
  
  # Sanity checks
  # 1. Converting NAs in var and group var to character
  .data <- .data %>%
    srvyr::mutate(dplyr::across(dplyr::any_of(c(var, group)), stringr::str_replace_na))
  
  # Ungrouped summary
  # Creating list of responses
  list_of_responses <- .data %>%
    srvyr::pull(var) %>%
    unique()
  
  # Iterating over list of responses, with 1 / 0 variables
  summary_data <- purrr::map_dfr(list_of_responses, function(x) {
    .data %>%
      srvyr::mutate(`____TEMPVAR____` = (!!rlang::sym(var) == x) * 1) %>%
      srvyr::summarise(value = sum_fnc(`____TEMPVAR____`,
                                       vartype = c("se", "ci"),
                                       na.rm = TRUE,
                                       level = level,
                                       proportion = TRUE),
                       N = srvyr::unweighted(sum(`____TEMPVAR____`))) %>%
      srvyr::mutate(group = "Overall",
                    response = x) %>%
      srvyr::select(group, response, N, value, everything())
  }) %>% 
    dplyr::arrange(-value)
  
  # Grouped summary
  if (length(group) > 0) {
    grouped_data <- purrr::map_dfr(group, function(group_var) {
      
      list_of_responses <- .data %>%
        dplyr::pull(var) %>%
        unique()
      
      # Summarising
      purrr::map_dfr(list_of_responses, function(x) {
        .data %>%
          srvyr::group_by(!!rlang::sym(group_var)) %>%
          srvyr::mutate(`____TEMPVAR____` = (!!rlang::sym(var) == x) * 1) %>%
          srvyr::summarise(value = sum_fnc(`____TEMPVAR____`,
                                           vartype = c("se", "ci"),
                                           na.rm = TRUE,
                                           level = level,
                                           proportion = TRUE),
                           N = srvyr::unweighted(sum(`____TEMPVAR____`))) %>%
          srvyr::mutate(group = case_when(
            # If more than one groups are provided, attach a group name finder, else just the normal works
            length(group) > 1 ~ paste0(group_var, " | ", !!rlang::sym(group_var)),
            TRUE ~ as.character(!!rlang::sym(group_var))
          ),
          response = x) %>%
          srvyr::select(group, response, N, value, everything(), - !!rlang::sym(group_var))
      }
      ) %>%
        dplyr::arrange(group)
    }
    )
    
    summary_data <- dplyr::bind_rows(summary_data,
                                     grouped_data)
  }
  
  return(summary_data)
}



#' @importFrom magrittr "%>%"
test_significance <- function(.data,
                              compare = compare) {
  
  # Function to test significance of a given mean against comparable or default values. Uses welch t test
  # TODO: Change default compare row by type; if numeric use the overall row, and if categorical,
  # use the row that meets the response
  
  # CHECK if data is 0 rows, return data
  if (nrow(.data) == 0) return(.data)
  
  # convert compare to lowercase, and trim ws
  compare <- trimws(compare) %>%
    tolower()
  
  # if compare is overall, pre-pend with "overall::"
  if (compare == "overall") {
    compare <- .data %>%
      dplyr::filter(group == "Overall") %>%
      dplyr::pull(response) %>%
      unique() %>%
      trimws() %>%
      tolower() %>%
      paste0("overall::", .)
  }
  
  # compare <- str_split(compare, "::") %>%
  #   unlist()
  
  # comparison row
  compare_row <- .data %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(c("group", "response")), ~tolower(trimws(.)))) %>%
    dplyr::mutate(tmp = glue::glue("{group}::{response}")) %>%
    dplyr::filter(tmp %in% compare) %>%
    dplyr::select(tmp_response = response,
                  compareN = N, compareV = value, compareVSE = value_se)
  
  # if compare row is null, push out a warning and use overall instead
  if (nrow(compare_row) == 0) {
    rlang::warn(glue::glue("The selected comparision does not exist in the data, defaulting to overall"))
    
    return(test_significance(.data, "overall"))
  }
  
  # If only one row is selected, it should be matched to all. If not then match by response
  if (nrow(compare_row) == 1) {
    # If only one single row is slected to be compared againgst
    significance_data <- .data %>%
      dplyr::mutate(compareN = compare_row$compareN,
                    compareV = compare_row$compareV,
                    compareVSE = compare_row$compareVSE) %>%
      dplyr::mutate(tmp_group = tolower(group),
                    tmp_response = tolower(response),
                    tmp_match = tolower(glue::glue("{tmp_group}::{tmp_response}")))
  } else {
    significance_data <- .data %>%
      dplyr::mutate(tmp_group = tolower(group),
                    tmp_response = tolower(response),
                    tmp_match = tolower(glue::glue("{tmp_group}::{tmp_response}"))) %>%
      dplyr::left_join(compare_row, by = c("tmp_response"))
  }
  
  significance_data <- significance_data %>%
    dplyr::mutate(pvalue = purrr::pmap_dbl(list(N,
                                                value,
                                                value_se,
                                                compareN,
                                                compareV,
                                                compareVSE), function(n, m, se, n0, m0, se0) {
                                                  tmp <- welch_ttest(
                                                    m1 = m,
                                                    m2 = m0,
                                                    s1 = se,
                                                    s2 = se0,
                                                    n1 = n,
                                                    n2 = n0
                                                  )
                                                  
                                                  tmp$p_value
                                                }),
                  sgnf = dplyr::case_when(
                    # Highlight row being compared against
                    tmp_match %in% compare ~ "<<--|",
                    
                    # Significance stars
                    pvalue < 0.001 ~ "[***]",
                    pvalue < 0.05  ~ "[ **]",
                    pvalue < 0.1   ~ "[  *]",
                    TRUE           ~ "-----"
                  )
    ) %>%
    dplyr::select(-tmp_group,
                  -tmp_response,
                  -tmp_match,
                  -compareN,
                  -compareV,
                  -compareVSE)
  
  return(significance_data)
}

welch_ttest <- function(m1,
                        m2,
                        s1,
                        s2,
                        n1,
                        n2,
                        m0 = 0,
                        equal.variance = FALSE) {
  
  # correct for providing SE instead of SD
  s1 <- s1 * sqrt(n1)
  s2 <- s2 * sqrt(n2)
  
  if (equal.variance == FALSE) {
    se <- sqrt( (s1^2/n1) + (s2^2/n2) )
    # welch-satterthwaite df
    df <- ((s1^2/n1 + s2^2/n2)^2)/((s1^2/n1)^2/(n1 - 1) + (s2^2/n2)^2/(n2 - 1))
  } else {
    # pooled standard deviation, scaled by the sample sizes
    se <- sqrt((1/n1 + 1/n2) * ((n1 - 1) * s1^2 + (n2 - 1) * s2^2)/(n1 + n2 - 2) )
    df <- n1 + n2 - 2
  }
  
  t <- (m1 - m2 - m0)/se
  
  dat = tibble::tibble(
    difference_of_means = (m1 - m2),
    std_error = se,
    t_value = t,
    p_value = 2 * pt(-abs(t), df)
  )
  
  return(dat)
}



