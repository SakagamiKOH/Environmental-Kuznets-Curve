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
    prep_date(data_order = "y") %>%
    ##重複しているデータがないかの確認
    prep_duplictae_count() %>% 
    ##スペル違い、スペース違いだが実際は同じことを指すデータ
    prep_gdp_synnoyms()
  
  save_interim(tidy_data, my_folder, extension = "tidy")
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
    ##データフレームを縦積みするから.id引数に"country_num"を指定して縦積みしたデータを区別する。
    dplyr::bind_rows(.id = "country_num") %>% 
    ##列を追加する。mutate(変数名=変数定義)
    dplyr::mutate(country = name_list[as.numeric(country_num)]) %>% 
    ##不要な列は削除する
    dplyr::select(-country_num)
  
  return(data_output)
}


##order the input data by date
prep_date <- function(data_input, data_order){
  data_output <- data_input %>% 
    ##mutate関数は変数列を追加する関数で、mutate(変数名=変数定義)で使う
    dplyr::mutate(
      ##lubridate::parse_date_time関数はデータを日付に解析(parse)する
      ##https://kazutan.github.io/SappoRoR8/01-lubridate.html
      data_formatted = lubridate::parse_date_time(
        data_input$year,
        order = data_order
      ) 
    ) 
    

  return(data_output)
}


##check duplication
prep_duplictae_count <- function(data_input){
  
  data_output <- data_input %>% 
    ##data_inputをcountry, data_formattedごとに細分化
    dplyr::group_by(country, data_formatted) %>% 
    ##上記細分化したグループの行数を数える
    dplyr::mutate(duplicate_id = dplyr::row_number()) %>% 
    ##細分化したデータの統合
    dplyr::ungroup()

  
  return(data_output)
}
##if there is no double count duplicate_id column will be 1


##omit the data that is double counted
prep_gdp_synnoyms <- function(data_input){
  data_output <- data_input %>% 
    ##まず新たにcountry_rename列を作り、その列全要素を"country"
    dplyr::mutate(country_rename = "country",
                  ##replace(original data, replaceされるもの, replaceするもの)
                  ##http://cse.naro.affrc.go.jp/takezawa/r-tips/r/15.html
                  country_rename = replace(
                    ##country_renameのうち
                    country_rename,
                    ##"Japan", "JPN", "japan","Jpn"のものを
                    country %in% c("Japan", "JPN", "japan","Jpn"),
                    ##"Japan"に置換する
                    "Japan"
                  ),
                  country_rename = replace(
                    country_rename,
                    country %in% c("the US", "The US", "United States",
                                          "UnitedStates", "the United States",
                                          "America"),
                  "United States"))
    
  return(data_output)
}


##box::use(`functions`/basic)
source("01_admin/functions/basics.R")

main()
