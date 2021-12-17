main <- function(){
  ##参照したいフォルダー名
  my_folder <- "gdp"
  ##参照したいファイル名のリスト
  name_list <- c("Japan", 
                 "United States")
  
  ##read raw data
  raw_data <- read_raw(my_folder, name_list)
  
  ##raw dataをきれいにする
  tidy_data <- raw_data %>% 
    ##日付順で並び替える
    prep_data(data_order = "ymd") %>%
    ##重複しているデータがないかの確認
    prep_duplictae_count() %>% 
    ##
    prep_gdp_synnoyms()
  
  basics$save_interim(tidy_data, my_folder, extension = "tidy")
}


##read raw data
read_raw <- function(my_folder, name_list){
  
  data_list <- name_list %>%  
    
    ##この段階では参照パスを出す
    
    ## purr::mapに関する解りやすい説明　https://qiita.com/kilometer/items/b4977df268d2c21211fc
    ##要はforループをより簡潔に行っている。
    purrr::map(function(name) 
      ## hereは　ファイル参照に便利　https://github.com/jennybc/here_here
      ##02_raw/my_folder/data/name.csv　ファイルを参照
      here::here("02_raw", my_folder,"data", paste0(name, ".csv"))
                 ) %>% 
    
    ##この段階ではパスからデータを読み込む
    
    purrr::map(readr::read_csv)
  
  data_output <- data_list %>% 
    ##データフレームを縦積みするから、.id引数に"country_num"を指定して縦積みしたデータを区別する。
    dplyr::bind_rows(.id = "country_num") %>% 
    ##列を追加する。mutate(変数名=変数定義)
    dplyr::mutate(country = name_list[as.numeric(country_num)]) %>% 
    ##不要な列は削除する
    dplyr::select(-country_num)
  
  return(data_output)
}