main <- function(){
  box::use(`functions`/basics)

}

#記述統計の作成
#記述統計は、分析後に行う？
#分析上、日本とアメリカに分ける
# 
# View(master_data)
# install.packages("skimr")
# library(skimr)
# descriptive_statistics<-skimr::skim(master_data)
# View(descriptive_statistics)

#分析上 国名を除いたデータを作る
delete_country_name_US_master<- US_master%>%
  dplyr::select(-country)

delete_country_name_US_master

delete_country_name_JPN_master<- JPN_master%>%
  dplyr::select(-country)

delete_country_name_JPN_master

install.packages("summarytools")
library("summarytools")
US_master_description<-delete_country_name_US_master%>%
  summarytools::descr(var=,stats = c("mean", "sd", "min", "max", "n.valid"),
                      transpose = TRUE, headings = FALSE)

JPN_master_description<-delete_country_name_JPN_master%>%
  summarytools::descr(var=,stats = c("mean", "sd", "min", "max", "n.valid"),
                      transpose = TRUE, headings = FALSE)

#出力
write.csv(US_master_description,"04_analyze/initial/table/US_description.csv")
write.csv(US_master_description,"04_analyze/initial/table/JPN_description.csv")


#散布図を描く
install.packages("ggplot2")
library(ggplot2)
# #rlangのバージョンが古い問題
# library(rlang)
# update.packages("rlang")
# remove.packages(rlang)

delete_country_name_JPN_master
delete_country_name_JPN_master%>%
  ggplot2::ggplot()+
  ggplot2::geom_point(aes(x=japan_population,y=gini))

delete_country_name_US_master
delete_country_name_US_master%>%
  ggplot2::ggplot()+
  ggplot2::geom_point(aes(x=us_population,y=gini))
main()

