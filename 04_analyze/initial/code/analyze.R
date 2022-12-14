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




#分析
#自己共分散を計算する
JPN<-delete_country_name_JPN_master
US<-delete_country_name_us_master
acf(JPN$gini,plot=T)
acf(JPN$japan_gdp_per_capita,plot=T)

acf(US$gini,plot=T)
acf(US$us_gdp_per_capita,plot=T)

install.packages("tseries")
library(tseries)

US
JPN
#一番基本的なモデル(タイムトレンドを考慮,SE typeもhansenみて指定したい)
library(estimatr)
US_general_reg<-lm_robust(gini~us_gdp_per_capita+I(us_gdp_per_capita^2)+year,data=US)
print(US_general_reg)

JPN_general_reg<-lm_robust(gini~japan_gdp_per_capita+I(japan_gdp_per_capita^2)+year,data=JPN)
print(JPN_general_reg)
#異種分散性テスト
install.packages("lmtest")
lmtest::bptest(US_general_reg)
lmtest::bptest(JPN_general_reg)
#異種分散性が見られるため、SE_type=HC3で計算する
US_general_reg<-lm_robust(gini~us_gdp_per_capita+I(us_gdp_per_capita^2)+year,data=US,se_type="HC3")
print(US_general_reg)

JPN_general_reg<-lm_robust(gini~japan_gdp_per_capita+I(japan_gdp_per_capita^2)+year,data=JPN,se_type="HC3")
print(JPN_general_reg)



#ラグ変数を追加するには、dplyrのラグで作れる


#トレンド制御　系列相関にロバストな回帰（FGLS

US_general_reg<-lm_robust(gini~us_gdp_per_capita+I(us_gdp_per_capita^2)+year,data=US,se_type="HC3")
print(US_general_reg)

JPN_general_reg<-lm_robust(gini~japan_gdp_per_capita+I(japan_gdp_per_capita^2)+year,data=JPN,se_type="HC3")
print(JPN_general_reg)

#回帰してから誤差項をadfテストするのでは？
tseries::adf.test(delete_country_name_US_master$gini,k=1)
#異種分散性テスト
#dickyfuller



main()

