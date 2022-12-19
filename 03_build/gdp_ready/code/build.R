#gdp_ready
#gdpのデータは5ねんずつで、日本もアメリカも欠損していないので問題ない
gdp_data <- gdp_data %>%
  dplyr::mutate(us_gdp_per_capita = us_gdp/us_population,
         japan_gdp_per_capita = japan_gdp/japan_population)
gdp_data

#output
output_dir_path <- here::here("03_build/gdp_ready/output")
file_path <- here::here(output_dir_path,"gdp_ready.csv")
write.csv(x=gdp_data,file = file_path ,row.names = FALSE)
