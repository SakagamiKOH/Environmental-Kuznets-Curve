main <- function(){
  my_folder <- "gdp"
  tidy_data <- read_interim(my_folder, extension = "tidy")
  
  ready_data <- tidy_data %>% 
    ##generate GDP per capita
    gen_gdp_per_capita()
  
  save_interim(ready_data, my_folder, extension = "ready")
}

gen_gdp_per_capita <- function(data_input){
  
  data_output <- data_input %>% 
    dplyr::mutate(gdp_per_cap = GDP/population)
  
  return(data_output)
}

source("01_admin/functions/basics.R")

main()
