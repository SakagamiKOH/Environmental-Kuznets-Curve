##
check_quantitative <- function(data, var_vector, folder_name){

  source("01_admin/functions/elucidate_quantile.R")
  
  file_name <- paste0(folder_name, "_check")
  
  ##下記参照
  ##https://qiita.com/kilometer/items/b4977df268d2c21211fc
  ##listを変数に取り、elucidate_quantile()を繰り返す
  purrr::map(var_vector, 
             function(var){
               elucidate_quantile$elucidate_quantile(
                 data = data,
                 var_name = (!!as.name(var)), 
                 output_folder = file_name)
             })
  
}

check_categorical <- function(data, var_vector, folder_name){
  source("01_admin/functions/elucidate_table.R")
  
  file_name <- paste0(folder_name, "_check")
  
  ##下記参照
  ##https://qiita.com/kilometer/items/b4977df268d2c21211fc
  ##listを変数に取り、elucidate_table()を繰り返す
  purrr::map(var_vector,
             function(var){
               elucidate_table(data,
                               (!!as.name(var)),
                               file_name)
             })
}

