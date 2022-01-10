main <- function(){
  my_folder_name <- "master"
  ##データの読み込み
  my_data <- read_interim(my_folder_name)
  main_varnames <- c("(Intercept)" = "Intercept",
                     "gdp_per_cap" = "gdp_per_capita",
                     "I(gdp_per_cap^2)" = "(gdp_per_capita)^2")
  output_folder <- "initial"
  
  
  ##記述統計量の作り方が不明
  ##my_data %>% 
  ##  summarize_data()
  
  
  my_data %>% 
    ##returns reg outcome as a list
    run_regression() %>% 
    ##change that list as a format table, then save it
    format_and_save_table(
      my_file_name = "initial_reg",
      my_title = "Initial regression",
      ##回帰結果の表の説明を一部変更
      my_varnames = main_varnames,
      my_folder = "initial"
    )
  
  my_data %>% 
    ##make scatter polt grouped by group_var
    run_scatter(
      x_var = gdp_per_cap,
      y_var = pollution,
      group_var = country
    ) %>% 
    save_my_plot(var_name = "Environmenta Kuznets Curve",
                 folder_name = my_folder_name)
}


run_regression <- function(data_input){
  ##save regression outcome into list, then return that list. 
  estimate_list <- list(
    "OLS" = estimatr::lm_robust(
      pollution ~ gdp_per_cap + I(gdp_per_cap^2),
      clusters = country, se_type = "stata",
      data = data_input
    ),
    
    ##FEはFixed Effectの略
    "FE" = estimatr::lm_robust(
      pollution ~ gdp_per_cap + I(gdp_per_cap^2),
      fixed_effects = ~ country,
      clusters = country, se_type = "stata",
      data = data_input
    )
  )
  
  return(estimate_list)
}


format_and_save_table <- function(estimates_lists, 
                                  my_file_name,
                                  my_title,  
                                  my_folder,
                                  my_varnames
                                  ){
  
  ##formatとパスを指定
  my_file_tex0 <- paste0(my_file_name, ".text")
  my_file_html0 <- paste0(my_file_name, ".html")
  my_file_tex <- here::here("04_analyze", my_folder,"table", my_file_tex0)
  my_file_html <- here::here("04_analyze",my_folder,"table",my_file_html0)
  
  my_content <- "^R2$|Std.Errors"
  ##Define my format
  my_fmt <- "%.2f"
  my_rows <- tibble::tribble(~term, ~'OLS', ~'FE', 'Clustering','Y','Y')
  attr(my_rows, 'position') <- 5
  
  table_tex <- modelsummary::msummary(
    estimates_lists,
    gof_omit = my_content,
    fmt = my_fmt, ##defined my format above
    title = my_title,
    coef_map = my_varnames,
    add_rows = my_rows,
    output = "latex", booktabs = TRUE
  ) %>% 
    ##define this function below
    format_table() %>% 
    ##define this function below
    add_double_lines_latex()
  writeLines(table_tex, my_file_tex)
  
  table_image <- modelsummary::msummary(
    estimates_lists,
    gof_omit = my_content,
    fmt = my_fmt, title = my_title,
    coef_map = my_varnames,
    add_rows = my_rows,
    output = "html"
  ) %>% 
    ##define this function below
    format_table() %>% 
    cat(.,file = my_file_html)
}

format_table <- function(table_input){
  table_output <- table_input %>% 
    kableExtra::kable_styling(bootstrap_options = c("hover", "condensed")) %>% 
    kableExtra::add_header_above(c(" " = 1, "(1)" = 1,"(2)" = 1 )) %>% 
    kableExtra::kable_classic_2(full_width = F) %>% 
    kableExtra::footnote(general = "Heteroskedasticity-robust standard error clustered at countries level are reported in the parenthesis.", 
                        threeparttable = T)
  
  return(table_output)
}

add_double_lines_latex <- function(table_input){
  table_output <- table_input %>% 
    sub("\\\\toprule","\\\\midrule\\\\midrule",.) %>% 
    sub("\\\\bottomrule","\\\\midrule\\\\midrule",.)
  
  return(table_output)
}

run_scatter <- function(data_input, x_var, y_var, group_var){
  x_var <- rlang::enquo(x_var)
  y_var <- rlang::enquo(y_var)
  group_var <- rlang::enquo(group_var)
  
  require(ggplot2)
  
  plot_output <- ggplot(
    data = data_input,
    mapping = aes(x = !!x_var, ## !!argumentについてはhttps://psych252.github.io/psych252book/figures/cheatsheets/tidyeval.pdfを参照。
                  y = !!y_var, ##!!argumetntとした方が人間にとっての可読性が高い
                  group = !!group_var,
                  color = !!group_var)
  ) +
    geom_point()
  
  return(plot_output)
}


source("01_admin/functions/basics.R")
source("01_admin/functions/elucidate_table.R")
library(magrittr)

main()