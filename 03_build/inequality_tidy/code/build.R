#inequality_tidy

raw_gini_path <- here::here("02_raw/inequality/data/Gini.xlsx")
raw_gini_data <- readxl::read_excel(raw_gini_path,col_names = FALSE,skip=1)

#long型へ変換する
delete_first_col_gini_data <- raw_gini_data %>%
  dplyr::select(-1)
delete_first_col_gini_data
trace_gini_data <- t(delete_first_col_gini_data)
trace_gini_data
colnames(trace_gini_data) <- c("country","year","gini")
tibble_gini_data <- trace_gini_data %>% 
  as_tibble()
tibble_gini_data

#missingをNAに置換
missing_to_NA_gini_data <- tibble_gini_data %>%
  dplyr::mutate(gini=ifelse(gini=="missing",NA,gini))
    

#gini,yearがcharacter型
long_gini_data <- missing_to_NA_gini_data
long_gini_data$gini
#NAs introduced by coercion という警告が出る
long_gini_data$gini <- as.numeric(long_gini_data$gini,na.rm=TRUE)
long_gini_data$year<- as.numeric(long_gini_data$year,na.rm=TRUE)
long_gini_data

#output
output_dir_path <- here::here("03_build/inequality_tidy/output")
file_path <- here::here(output_dir_path,"inequality_tidy.csv")
write.csv(x=long_gini_data,file = file_path ,row.names = FALSE)

 