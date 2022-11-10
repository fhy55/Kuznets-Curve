#gdp_ready

raw_japan_gdp_path <- here::here("C:/01-R/HW/Kuznets-Curve-main/Kuznets-Curve/02_raw/gdp/data/Japan.csv")
raw_japan_gdp_data <- readr::read_csv(raw_japan_gdp_path)

raw_us_gdp_path <- here::here("C:/01-R/HW/Kuznets-Curve-main/Kuznets-Curve/02_raw/gdp/data/United States.csv")
raw_us_gdp_data <- readr::read_csv(raw_us_gdp_path)

View(raw_japan_gdp_data)
view(raw_us_gdp_data)


#型を確認する
install.packages("summarytools")
library(summarytools)
summary_raw_japan_data <- dfSummary(raw_japan_gdp_data)
view(summary_raw_japan_data)

summary_raw_us_data <- dfSummary(raw_us_gdp_data)
view(summary_raw_us_data)


#それぞれのデータの変数名に国名を加える
japan_gdp_data <- raw_japan_gdp_data %>%
  dplyr::rename(japan_population = population, japan_gdp = GDP)


us_gdp_data <- raw_us_gdp_data %>%
  dplyr::rename(us_population = population, us_gdp = GDP)

#yearをkeyとして結合する
gdp_data <- dplyr::full_join(japan_gdp_data, us_gdp_data)
View(gdp_data)

