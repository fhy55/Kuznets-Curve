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
US_master<- US_master%>%
  dplyr::select(-country)%>%
  dplyr::rename(gdp=us_gdp,population=us_population,gdp_per_capita=us_gdp_per_capita)


JPN_master<- JPN_master%>%
  dplyr::select(-country)%>%
  dplyr::rename(gdp=japan_gdp,population=japan_population,gdp_per_capita=japan_gdp_per_capita)

JPN_master
US_master

install.packages("summarytools")
library("summarytools")
#記述統計
library("kableExtra")

#平均,分散,データ数,最大値,最小値
tribble(
  ~Name,     ~Econ, ~Math,
  "Alice",      90,   mean(100),
  "Bob",        80,    90,
  "Charlie",   100,    60
)
sd(US_master$gini)
n(US_master$gini)
US_description<-tribble(
  ~variables,    ~explanation,     ~mean,        ~std.dev,      ~observation,
  "gini",   "ジニ指数"    , round(mean(US_master$gini),2)   , round(sd(US_master$gini),2) , length(US_master$gini),
  "poplulation","人口",        round(mean(US_master$population),2)   , round(sd(US_master$population),2),length(US_master$population),
  "gdp_per_capita", "一人当たりGDP" ,mean(US_master$gdp_per_capita)   , sd(US_master$gdp_per_capita),length(US_master$gdp_per_capita),
  "year","年",                round(mean(US_master$year),2)    , round(sd(US_master$year),2),length(US_master$year)
)

JPN_description<-tribble(
  ~variables,    ~explanation,     ~mean,        ~std.dev,      ~observation,
  "gini",   "ジニ指数"    , round(mean(JPN_master$gini),2)   , round(sd(JPN_master$gini),2) , length(JPN_master$gini),
  "poplulation","人口",        round(mean(JPN_master$population),2)   , round(sd(JPN_master$population),2),length(JPN_master$population),
  "gdp_per_capita", "一人当たりGDP" ,mean(JPN_master$gdp_per_capita)   , sd(JPN_master$gdp_per_capita),length(JPN_master$gdp_per_capita),
  "year","年",                round(mean(JPN_master$year),2)    , round(sd(JPN_master$year),2),length(JPN_master$year)
)
mean(US_master$gdp_per_capita)

library(kableExtra)
kableExtra::kbl(US_description)%>%
  kableExtra::kable_styling(full_width=FALSE)%>%
  kable_classic_2()
kableExtra::kbl(JPN_description)%>%
  kableExtra::kable_styling(full_width=FALSE)%>%
  kable_classic_2()

US_master_description<-US_master%>%
  summarytools::descr(var=,stats = c("mean", "sd", "min", "max", "n.valid"),
                      transpose = TRUE, headings = FALSE)

JPN_master_description<-JPN_master%>%
  summarytools::descr(var=,stats = c("mean", "sd", "min", "max", "n.valid"),
                      transpose = TRUE, headings = FALSE)

US_master_description
JPN_master_description
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

JPN_master
JPN_master%>%
  ggplot2::ggplot()+
  ggplot2::geom_point(aes(x=gdp_per_capita,y=gini))+
  labs(title="【US】x:gdp per capita y:gini ",subtitle="scatter plot",y="gini",x="gdp per capita")+
  theme(
    plot.title    = element_text(face = "bold", color = "blue"), 
    axis.title.x  = element_text(hjust = 20), 
    axis.title.y  = element_text(size = 20), 
    plot.subtitle = element_text(color = "blue", size = 12),
  )

US_master%>%
  ggplot2::ggplot()+
  ggplot2::geom_point(aes(x=gdp_per_capita,y=gini))

#時系列変化

US_master%>%
  ggplot2::ggplot(aes(x=year,y=gdp_per_capita))+
  ggplot2::geom_line(size=1)+
  ggplot2::geom_point()+
  labs(title="【US】gdp per capita",subtitle="time series",y="gdp per capita")+
  theme(
    plot.title    = element_text(face = "bold", color = "blue"), 
    axis.title.x  = element_text(hjust = 20), 
    axis.title.y  = element_text(size = 20), 
    plot.subtitle = element_text(color = "blue", size = 12),
  )+
  scale_x_continuous(breaks = seq(US_master$year[1],US_master$year[length(US_master$year)],5))
JPN_master%>%
  ggplot2::ggplot()+
  ggplot2::geom_line(aes(x=year,y=gdp_per_capita))
US_master%>%
  ggplot2::ggplot()+
  ggplot2::geom_line(aes(x=year,y=gini))
JPN_master%>%
  ggplot2::ggplot()+
  ggplot2::geom_line(aes(x=year,y=gini))


#分析
#自己共分散を計算する

acf(JPN_master$gini,plot=T)
acf(JPN_master$gdp_per_capita,plot=T)

acf(US_master$gini,plot=T)
acf(US_master$gdp_per_capita,plot=T)

install.packages("tseries")
library(tseries)

#一番基本的なモデル(タイムトレンドを考慮,SE typeもhansenみて指定したい)
library(estimatr)
US_general_reg<-lm_robust(gini~gdp_per_capita+I(gdp_per_capita^2)+year,data=US_master)
print(US_general_reg)

JPN_general_reg<-lm_robust(gini~gdp_per_capita+I(gdp_per_capita^2)+year,data=JPN_master)
print(JPN_general_reg)
#異種分散性テスト
install.packages("lmtest")
lmtest::bptest(US_general_reg)
lmtest::bptest(JPN_general_reg)

#異種分散性が見られるため、SE_type=HC3で計算する
US_general_reg<-lm_robust(gini~gdp_per_capita+I(gdp_per_capita^2)+year,data=US_master,se_type="HC3")
print(US_general_reg)

JPN_general_reg<-lm_robust(gini~gdp_per_capita+I(gdp_per_capita^2)+year,data=JPN_master,se_type="HC3")
print(JPN_general_reg)

#model_summary
install.packages("modelsummary")
library(modelsummary)
regs <- list(US_general_reg,JPN_general_reg)
msummary(regs)

#固定効果モデル(一階差分を取ることによって単位根過程を対策できる)
JPN_master
diff_gdp_per_capita<-JPN_master$gdp_per_capita-lag(JPN_master$gdp_per_capita,1)
JPN_master$gdp_per_capita
lag(JPN_master$gdp_per_capita,2)
JPN_master$gdp_per_capita-lag(JPN_master$gdp_per_capita)
diff_gdp_per_capita
difference_JPN_master<-JPN_master%>%
  mutate(diff_gdp_per_capita=gdp_per_capita-lag(gdp_per_capita),year=year-lag(year),gini=gini-lag(gini))
difference_JPN_master
install.packages("plm")
library(plm)
f<-"gini ~ 0 + gdp_per_capita+I(gdp_per_capita^2)+year"
f<-as.formula(f)
e<-plm(formula=f,data=JPN_master,effect="individual",model="fd",index=c("year"))
summary(e)

#ラグ変数を追加するには、dplyrのラグで作れる

#追加の時系列分析は、STATAで行う
write.csv(US_master,"04_analyze/initial/table/US_master.csv")
write.csv(JPN_master,"04_analyze/initial/table/JPN_master.csv")


#トレンド制御　系列相関にロバストな回帰（FGL

US_general_reg<-lm_robust(gini~gdp_per_capita+I(gdp_per_capita^2)+year,data=US_master,se_type="HC3")
print(US_general_reg)

JPN_general_reg<-lm_robust(gini~gdp_per_capita+I(gdp_per_capita^2)+year,data=JPN_master,se_type="HC3")
print(JPN_general_reg)

#回帰してから誤差項をadfテストするのでは？
tseries::adf.test(US_general_reg,k=1)
#異種分散性テスト
#dickyfuller



main()

