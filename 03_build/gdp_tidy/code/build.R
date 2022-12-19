#gdp_ready

raw_japan_gdp_path <- here::here("02_raw/gdp/data/Japan.csv")
raw_japan_gdp_data <- readr::read_csv(raw_japan_gdp_path)

raw_us_gdp_path <- here::here("02_raw/gdp/data/United States.csv")
raw_us_gdp_data <- readr::read_csv(raw_us_gdp_path)



#それぞれのデータの変数名に国名を加える
japan_gdp_data <- raw_japan_gdp_data %>%
  dplyr::rename(japan_population = population, japan_gdp = GDP)


us_gdp_data <- raw_us_gdp_data %>%
  dplyr::rename(us_population = population, us_gdp = GDP)

#yearをkeyとして結合する
gdp_data <- dplyr::full_join(japan_gdp_data,
                             us_gdp_data)

#output
output_dir_path <- here::here("03_build/gdp_tidy/output")
file_path <- here::here(output_dir_path,"gdp_tidy.csv")
write.csv(x=gdp_data,file = file_path ,row.names = FALSE)
