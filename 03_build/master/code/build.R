#master
master_data<-convine_japan_us_gini%>%
  dplyr::inner_join(gdp_data,by="year","country")

#これはwide形式になっている？
master_data
JPN_master<-master_data%>%
  dplyr::filter(country=="JPN")%>%
  dplyr::select(year,country,gini,japan_population,japan_gdp,japan_gdp_per_capita)
JPN_master

#日本関連の列を消す japanese_populationなど
US_master<-master_data%>%
  dplyr::filter(country=="USA")%>%
  dplyr::select(year,country,gini,us_population,us_gdp,us_gdp_per_capita)
US_master

output_dir_path <- here::here("03_build/master/output")
file_path <- here::here(output_dir_path,"master.csv")
write.csv(x=master_data,file = file_path ,row.names = FALSE)

