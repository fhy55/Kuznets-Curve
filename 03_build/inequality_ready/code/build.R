
nrow_gini<- nrow(long_gini_data)
nrow_gini


#間のない年を補填する.取り扱いやすくするためにUSと日本で分ける
japan_gini <- subset(long_gini_data,long_gini_data$country=="JPN")
japan_gini
us_gini <- subset(long_gini_data,long_gini_data$country=="USA")
us_gini

us_gini<-complete(us_gini, year = full_seq(year, 1))
japan_gini<-complete(japan_gini, year = full_seq(year, 1))

us_gini$country <-"USA"
us_gini
japan_gini$country <-"JPN"
japan_gini

japan_gini$gini <- na.approx(japan_gini$gini)


us_gini$gini <- na.approx(us_gini$gini)


japan_gini_multiple_5 <- japan_gini%>%
  dplyr::filter(year %% 5==0)
japan_gini_multiple_5

us_gini_multiple_5 <- us_gini%>%
  dplyr::filter(year %% 5==0)
us_gini_multiple_5

#5年ずつ抽出する
convine_japan_us_gini <- us_gini_multiple_5%>%
  dplyr::full_join(japan_gini_multiple_5)

convine_japan_us_gini
#output
output_dir_path <- here::here("03_build/inequality_ready/output")
file_path <- here::here(output_dir_path,"inequality_ready.csv")
write.csv(x=convine_japan_us_gini,file = file_path ,row.names = FALSE)
