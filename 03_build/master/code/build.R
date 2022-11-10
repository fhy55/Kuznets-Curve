
master_data<-convine_japan_us_gini%>%
  inner_join(gdp_data,by="year","country")

master_data
JPN_master<-master_data%>%
  filter(country=="JPN")%>%
  select(year,country,gini,japan_population,japan_gdp,japan_gdp_per_capita)
JPN_master

#日本関連の列を消す japanese_populationなど
US_master<-master_data%>%
  filter(country=="USA")%>%
  select(year,country,gini,us_population,us_gdp,us_gdp_per_capita)
US_master


#何度も変換していて心配,最初から日本とアメリカ別のままやればよかったのでは。


