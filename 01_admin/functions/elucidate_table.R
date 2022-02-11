elucidate_table <- function(data, var_name, output_folder){
  
  var_name_rlang <- rlang::enquo(var_name)
  
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
    dplyr::select(!!var_name_rlang) %>% 
    ##separate data by variable
    
    dplyr::group_by(country) %>% 
    ##summarize data set by group that user made above
    dplyr::summarise(Q1_gdp_per_cap = quantile(gdp_per_cap, probs = 0.25),
                     median_gdp_per_cap = median(gdp_per_cap),
                     mean_gdp_per_cap = mean(gdp_per_cap),
                     Q3_gdp_per_cap = quantile(gdp_per_cap, probs = 0.75),
                     Q1_pollution = quantile(pollution, probs = 0.25),
                     median_pollution = median(pollution),
                     mean_pollution = mean(pollution),
                     Q3_pollution = quantile(pollution, probs = 0.75)
                     ) 

  
  
  return(table_output)
  
}


##formatをtex形式に指定して、save the table
format_and_save_table <- function(my_table, var_name, output_folder){
  var_name <- rlang::enquo(var_name)
  library(magrittr)
  # box::use(magrittr[`%>%`])
  
  ##tableのファイル形式を指定(今回はtext形式)
  my_file_tex0 <- paste0(var_name, ".tex")  
  varname_id <- 2
  ##tableの保存先へのパスを入力
  my_file_tex <- here::here("04_analyze", output_folder, "table", my_file_tex0[varname_id])

  ##latexにも使える形式へ。細かなテーブルの設定を行っている。
  table_tex <- my_table %>% 
    kableExtra::kbl(format = "latex", booktabs = T) %>% 
    kableExtra::kable_styling(latex_options = "hold_position",
                  full_width = F) %>% 
    kableExtra::kable_classic_2(full_width = F) 
  writeLines(table_tex, my_file_tex)
  
  
  ####tableのファイル形式を指定(今回はhtlm形式)
  my_file_html0 <- paste0(var_name, ".html")  
  ##tableの保存先へのパスを入力
  my_file_html <- here::here("04_analyze", output_folder, "table", my_file_html0[varname_id])

  ##htlm形式のテーブルの細かな見栄えの設定を行っている
  table_html <- my_table %>% 
    kableExtra::kbl(format = "html") %>% 
    kableExtra::kable_styling(bootstrap_options = "hover") %>%
    kableExtra::kable_classic_2(full_width = F) %>% 
    cat(., file = my_file_html)
}