main <- function(){
  my_folder <- "pollution"
  tidy_data <- read_interim(my_folder, extension = "tidy")
  
  ready_data <- tidy_data %>% 
    ##generate standard year table with interval = 5
    gen_yeartab(interval = 5) 

  save_interim(ready_data, my_folder, extension = "ready")
}


gen_yeartab <- function(data_input, interval){

  data_summarized <- data_input %>% 
    ##interval 年を基準に年を割り振る
    dplyr::mutate(group = round(as.numeric(year)/interval) * 
                    dplyr::if_else(country == "JPN", 
                                   true = 1, 
                                   false = 2, 
                                   missing = 3)) %>% 
    dplyr::group_by(group) %>% 
    dplyr::summarise(pollution = mean(pollution, na.rm = TRUE),
                     missing_rate = mean(missing_dummy)) %>% 
    dplyr::ungroup() 
  
  data_output <- data_input %>% 
    dplyr::mutate(quotient = as.numeric(year)/interval) %>% 
    dplyr::mutate(group = round(quotient) * 
                    dplyr::if_else(country == "JPN", 
                                   true = 1, 
                                   false = 2, 
                                   missing = 3)) %>% 
    ##intervalで割り切れる年の行だけ抽出
    dplyr::filter(quotient %% 1 == 0) %>% 
    dplyr::select(-pollution) %>% 
    dplyr::left_join(data_summarized, by = ("group" = "group")) %>% 
    dplyr::select(-c("group", "quotient","pollution_original","missing_dummy"))
  
  return(data_output)  
}


source("01_admin/functions/basics.R")

main()
