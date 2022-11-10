#gdp_ready
#gdpのデータは5ねんずつで、日本もアメリカも欠損していないので問題ない
gdp_data <- gdp_data %>%
  dplyr::mutate(us_gdp_per_capita = us_gdp/us_population,
         japan_gdp_per_capita = japan_gdp/japan_population)
gdp_data
