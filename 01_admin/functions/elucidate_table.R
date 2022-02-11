elucidate_table <- function(data, var_name, output_folder){
  
  library(magrittr)
  #box::use(magrittr[`%>%`])
  
  data %>% 
    ##generate table
    gen_table(var_name) %>% 
    ##save the table
    format_and_save_table(var_name, output_folder)
}


##generate table
gen_table <- function(data_input, var_name){
  
  
  var_name_rlang <- rlang::enquo(var_name)

  ##run the code below to use pipe operator
  library(magrittr)
  #box::use(magrittr[`%>%`])
  
  table_output <- data_input %>% 
    ## 分析対象の列のみ残す
    dplyr::select(!!var_name_rlang) %>% 
    ##separate data by variable
    dplyr::group_by(country) %>% 
    ##summarize data set by group that user made above
    dplyr::summarise(A_min = min(pollution),
                     B_first_quantile = quantile(pollution, probs = 0.25),
                     C_median = median(pollution),
                     D_mean = mean(pollution),
                     E_third_quantile = quantile(pollution, probs = 0.75),
                     F_max = max(pollution)
                     ) %>% 
    tidyr::gather(key = desc_stat_poll, value = value, -country) %>% 
    tidyr::spread(key = country, value = value) %>% 
    dplyr::mutate(JPN = round(JPN, digits = 2),
                  USA = round(USA, digits = 2))
  
  return(table_output)
  
}


##formatをtex形式に指定して、save the table
format_and_save_table <- function(my_table, var_name, output_folder){
  
  var_name_rlang <- rlang::enquo(var_name)
  library(magrittr)
  # box::use(magrittr[`%>%`])
  
  ##tableのファイル形式を指定(今回はtext形式)
  my_file_tex0 <- paste0(var_name, ".tex")  
  varname_id <- 2
  ##tableの保存先へのパスを入力
  my_file_tex <- here::here("04_analyze", output_folder, "table", my_file_tex0[varname_id])

  ##latexにも使える形式へ。細かなテーブルの設定を行っている。
  table_tex <- my_table %>% 
    kableExtra::kbl(format = "latex", booktabs = TRUE) %>% 
    kableExtra::kable_styling(latex_options = "hold_position",
                  full_width = FALSE) %>% 
    kableExtra::kable_classic_2(full_width = FALSE) %>% 
    kableExtra::footnote(general = "summarise only pollution data.", 
                         threeparttable = T)
  writeLines(table_tex, my_file_tex)
  
  
  ####tableのファイル形式を指定(今回はhtlm形式)
  my_file_html0 <- paste0(var_name, ".html")  
  ##tableの保存先へのパスを入力
  my_file_html <- here::here("04_analyze", output_folder, "table", my_file_html0[varname_id])

  ##htlm形式のテーブルの細かな見栄えの設定を行っている
  table_html <- my_table %>% 
    kableExtra::kbl(format = "html") %>% 
    kableExtra::kable_styling(bootstrap_options = "hover",
                              full_width = FALSE) %>%
    kableExtra::kable_classic_2(full_width = FALSE) %>%
    kableExtra::footnote(general = "summarise only pollution data.", 
                         threeparttable = T)
    cat(., file = my_file_html)
}


