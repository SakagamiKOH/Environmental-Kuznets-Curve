main <- function(){
  my_folder <- "pollution"
  file_name <- "CO2"
  
  raw_data <- read_raw(my_folder, file_name)
  
  tidy_data <- raw_data %>% 
    prep_shape() %>% 
    prep_nonnumeric() %>% 
    ##check data type
    prep_asserts()
  
  save_interim(tidy_data, my_folder, extension = "tidy")
}


##read the raw data
read_raw <- function(my_folder, file_name){
  file_path <- here::here("02_raw", my_folder, "data", paste0(file_name, ".xlsx"))
  ##初めの2行は空なのでskipする
  data_output <- readxl::read_excel(file_path, skip = 2)
  
  return(data_output)
}


prep_shape <- function(data_input){
  data_output <- data_input %>% 
    
    ##列名を一部変更
    dplyr::rename(country = ...1) %>% 
    
    ##across(.col = 対象となる列, .fnd = 行いたい操作)
    ##つまり全ての列において文字型に変更した
    dplyr::mutate(across(everything(), as.character)) %>% 
    
    ##pivot_longer()はデータの行数を増やし、列数を減らす
    tidyr::pivot_longer(
      ##cols引数でロング形式データのpivotを指定
      cols = !country,
      ##変換後のpivot列名
      names_to = "year",
      ##names_prefixは指定した文字列を接頭辞（prefix）として変換後のデータに入れない
      names_prefix = "pollution_",
      ##pivotでない列の列名
      values_to = "pollution_original"
    ) 

  return(data_output)
}


##change pollution_original into numeric from character
prep_nonnumeric <- function(data_input){
  data_output <- data_input %>% 
    
    ##add new variable columns
    dplyr::mutate(
      
  　##missing_dummyをpollution_originalが"missing"またはNAのとき1,以外を0と定義
    missing_dummy = dplyr::if_else(
      pollution_original =="missing",
       true = 1, false = 0, missing = 0),
    
    ##pollutionを“missing”のときのみNA,以外をpollution_originalと定義
    pollution_numeric = replace(pollution_original,
                        which(pollution_original == "missing"),
                        NA),
    
    ##hange pollution_original into numeric from character
    ##ただしNAもnumericに分類されることに注意
    pollution_numeric = as.numeric(pollution_numeric),
    year = as.numeric(year))
  
  return(data_output)
}


##Check whether data type is correct
prep_asserts <- function(data_input){
  
  ##test_that(desc = "test name" {test code})
  testthat::test_that(desc = "data type correct",{
    testthat::expect_true(is.numeric(data_input$pollution_numeric))
    testthat::expect_true(is.numeric(data_input$missing_dummy))
  })
  
  return(data_input)
}


##import a module or package
source("01_admin/functions/basics.R")
##box::use(`functions`/basics)

main()
