main <- function(){
  my_folder <- "pollution"
  tidy_data <- read_interim(my_folder, extension = "tidy")
  
  ready_data <- tidy_data %>% 
    ##generate standard year table with interval = 5
    gen_yeartab(interval = 5) %>% 
    ##select raws with low missing rate
    select_lowmissing() 

  save_interim(ready_data, my_folder, extension = "ready")
}


gen_yeartab <- function(data_input, interval){
  variable <- rlang::enquo(year)
  
  data_output <- data_input %>% 
    ##interval 年で割り切れる年に絞り込む
    dplyr::mutate(group = round(year/interval, digits = 0)) %>% 
    dplyr::group_by(group) %>% 
    ##ここどうする？
    dplyr::summarise() %>% 
    dplyr::ungroup()
  
  return(data_output)  
}


source("01_admin/functions/basics.R")

main()