main <- function(){
  gdp_data <- read_interim("gdp", extension = "ready")
  pollution_data <- read_interim("pollution", extension = "ready")

  ##マージするためにプライマリーキーをそろえる(Type, 表記法など)
  gdp_data_for_merge <- prep_gdp(gdp_data)

  ##マージする
  master_data <- prep_merge(
    gdp_data_for_merge,
    pollution_data)
  
  save_interim(master_data, "master")
}


##結合のためにプライマリーキーを整えたgdpデータを作成
prep_gdp <- function(data_input, interval){
  data_output <- data_input %>% 
    dplyr::mutate(country = dplyr::if_else(country == "Japan", "JPN", country),
                  country = dplyr::if_else(country == "United States", "USA", country),
                  ) 
  
  return(data_output)
}


prep_merge <- function(gdp_data, pollution_data){
  data_output <- gdp_data %>% 
    ##by引数でプライマリーキーを対応付ける
    dplyr::left_join(pollution_data, by = c("country" = "country",
                                            "year" = "group_5year"))
  
  return(data_output)
}




source("01_admin/functions/basics.R")

main()