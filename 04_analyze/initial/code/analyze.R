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

US_master<-US_master%>%
  dplyr::mutate(lgdp=log(gdp),lpopulation=log(population),lgdp_per_capita=log(gdp_per_capita),lgini=log(gini))


JPN_master<-JPN_master%>%
  dplyr::mutate(lgdp=log(gdp),lpopulation=log(population),lgdp_per_capita=log(gdp_per_capita),lgini=log(gini))
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
length(US_master$gini)
US_description<-tribble(
  ~variables,    ~explanation,     ~mean,        ~std.dev,      ~observation,
  "gini",   "ジニ指数"    , round(mean(US_master$gini),2)   , round(sd(US_master$gini),2) , length(US_master$gini),
  "lpoplulation","人口の対数変換",        round(mean(US_master$lpopulation),2)   , round(sd(US_master$lpopulation),2),length(US_master$lpopulation),
  "lgdp", "GDPの対数変換" ,mean(US_master$lgdp)   , sd(US_master$lgdp),length(US_master$lgdp),
  "lgdp_per_capita", "一人当たりGDPの対数変換" ,mean(US_master$lgdp_per_capita)   , sd(US_master$lgdp_per_capita),length(US_master$lgdp_per_capita),
  "year","年",                round(mean(US_master$year),2)    , round(sd(US_master$year),2),length(US_master$year)
)

round(mean(US_master$lgini),2)
JPN_description<-tribble(
  ~variables,    ~explanation,     ~mean,        ~std.dev,      ~observation,
  "gini",   "ジニ指数"    , round(mean(JPN_master$gini),2)   , round(sd(JPN_master$gini),2) , length(JPN_master$gini),
  "lgdp", "GDPの対数変換" ,mean(JPN_master$lgdp)   , sd(JPN_master$lgdp),length(JPN_master$lgdp),
  "lpoplulation","人口の対数変換",        round(mean(JPN_master$lpopulation),2)   , round(sd(JPN_master$lpopulation),2),length(JPN_master$lpopulation),
  "lgdp_per_capita", "一人当たりGDPの対数変換" ,mean(JPN_master$lgdp_per_capita)   , sd(JPN_master$lgdp_per_capita),length(JPN_master$lgdp_per_capita),
  "year","年",                round(mean(JPN_master$year),2)    , round(sd(JPN_master$year),2),length(JPN_master$year)
)

# JPN_description<-tribble(
#   ~variables,    ~explanation,     ~mean,        ~std.dev,      ~observation,
#   "gini",   "ジニ指数"    , round(mean(JPN_master$gini),2)   , round(sd(JPN_master$gini),2) , length(JPN_master$gini),
#   "poplulation","人口",        round(mean(JPN_master$population),2)   , round(sd(JPN_master$population),2),length(JPN_master$population),
#   "gdp_per_capita", "一人当たりGDP" ,mean(JPN_master$gdp_per_capita)   , sd(JPN_master$gdp_per_capita),length(JPN_master$gdp_per_capita),
#   "lgini",   "ジニ指数"    , round(mean(JPN_master$lgini),2)   , round(sd(JPN_master$lgini),2) , length(JPN_master$lgini),
#   "lpoplulation","人口",        round(mean(JPN_master$lpopulation),2)   , round(sd(JPN_master$lpopulation),2),length(JPN_master$lpopulation),
#   "lgdp_per_capita", "一人当たりGDP" ,mean(JPN_master$lgdp_per_capita)   , sd(JPN_master$lgdp_per_capita),length(JPN_master$lgdp_per_capita),
#   "year","年",                round(mean(JPN_master$year),2)    , round(sd(JPN_master$year),2),length(JPN_master$year)
# )
mean(US_master$gdp_per_capita)

JPN_description

library(kableExtra)
US_description%>%
  kableExtra::kbl(digits = 2,caption="US 記述統計表")%>%
  kableExtra::kable_styling(full_width=FALSE)%>%
  kable_classic_2()
# 
# JPN_description<-knitr::kable(JPN_description, digits = 3, format.args = list(scientific = FALSE))

JPN_description%>%
  kableExtra::kbl(digits = 2,caption="JPN 記述統計表")%>%
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
  ggplot2::geom_point(aes(x=lgdp_per_capita,y=gini))+
  labs(title="【JPN】scatter plot ",subtitle="x:lgdp per capita y:gini",y="gini",x="lgdp per capita")+
  theme(
    plot.title    = element_text(face = "bold", color = "blue"), 
    axis.title.x  = element_text(size = 20), 
    axis.title.y  = element_text(size = 20), 
    plot.subtitle = element_text(color = "blue", size = 12),
  )

cor(JPN_master$gini,JPN_master$lgdp_per_capita)
cor(US_master$gini,US_master$lgdp_per_capita)

US_master%>%
  ggplot2::ggplot()+
  ggplot2::geom_point(aes(x=lgdp_per_capita,y=gini))+
  labs(title="【US】scatter plot ",subtitle="x:lgdp per capita y:gini",y="gini",x="lgdp per capita")+
  theme(
    plot.title    = element_text(face = "bold", color = "blue"), 
    axis.title.x  = element_text(size  = 20), 
    axis.title.y  = element_text(size = 20), 
    plot.subtitle = element_text(color = "blue", size = 12),
  )

#時系列変化

US_master%>%
  ggplot2::ggplot(aes(x=year,y=lgdp_per_capita))+
  ggplot2::geom_line(size=1)+
  ggplot2::geom_point()+
  labs(title="【US】gdp per capita",subtitle="time series",y="lgdp per capita")+
  theme(
    plot.title    = element_text(face = "bold", color = "blue"), 
    axis.title.x  = element_text(size  = 20), 
    axis.title.y  = element_text(size = 20), 
    plot.subtitle = element_text(color = "blue", size = 12),
  )+
  scale_x_continuous(breaks = seq(US_master$year[1],US_master$year[length(US_master$year)],5))

JPN_time_lgdp_per_capita<-JPN_master%>%
  ggplot2::ggplot(aes(x=year,y=lgdp_per_capita))+
  ggplot2::geom_line(size=1)+
  ggplot2::geom_point()+
  labs(title="【JPN】gdp per capita",subtitle="time series",y="lgdp per capita")+
  theme(
    plot.title    = element_text(face = "bold", color = "blue"), 
    axis.title.x  = element_text(size  = 20), 
    axis.title.y  = element_text(size = 20), 
    plot.subtitle = element_text(color = "blue", size = 12),
  )+
  scale_x_continuous(breaks = seq(JPN_master$year[1],JPN_master$year[length(JPN_master$year)],5))



US_master%>%
  ggplot2::ggplot(aes(x=year,y=gini))+
  ggplot2::geom_line(size=1)+
  ggplot2::geom_point()+
  labs(title="【US】gini",subtitle="time series",y="gini")+
  theme(
    plot.title    = element_text(face = "bold", color = "blue"), 
    axis.title.x  = element_text(size =   20), 
    axis.title.y  = element_text(size = 20), 
    plot.subtitle = element_text(color = "blue", size = 12),
  )+
  scale_x_continuous(breaks = seq(US_master$year[1],US_master$year[length(US_master$year)],5))

JPN_time_gini<-JPN_master%>%
  ggplot2::ggplot(aes(x=year,y=gini))+
  ggplot2::geom_line(size=1)+
  ggplot2::geom_point()+
  labs(title="【JPN】gini",subtitle="time series",y="gini")+
  theme(
    plot.title    = element_text(face = "bold", color = "blue"), 
    axis.title.x  = element_text(size = 20), 
    axis.title.y  = element_text(size = 20), 
    plot.subtitle = element_text(color = "blue", size = 12),
  )+
  scale_x_continuous(breaks = seq(JPN_master$year[1],JPN_master$year[length(JPN_master$year)],5))

#2軸グラフを作る




#変数のスケールを変形する

y1.lim <- c(-12, -9) #左軸 temperature
y2.lim <- c(0.3, 0.5) #右軸 Atmospheric_pressure

variable_scaler<- function(p, lim1, lim2){    #pとlim1とlim2に対する処理を{}にかく
  to_zero <- p-lim2[1]     #lim2[1]は、軸スケールの最小値。#4でlim2は、y2.lim（右軸）であり、y2.limの最小値は860。pは気圧の各数値。
  y1_range <- lim1[2]-lim1[1]     #lim1[2]はy1.limの最大値から、lim1[1]はy1.limの最小値を引いたものがy1.lim（左軸）幅ということ。
  y2_range <- lim2[2]-lim2[1]     #上記と同様y2.lim（右軸）の幅
  scaled <- to_zero*y1_range/y2_range    # 気圧の各数値から、最小値860をひく。これに、y1幅/y2幅をかける。y2（右軸）の幅表示をy1（左軸）の幅表示に合わせる作業。
  from_zero <- scaled + lim1[1]     # 左軸の最小値をゼロとするため、もし0以上であれば、その分y1の最小値を足しておく。
  return(from_zero)}   #2 この関数を使えば、数値の修正が簡単。

axis_scaler <- function(p, lim1, lim2){   #pは#4でいうところの.になる。.は左軸情報を表す。左軸とlim1とlim2に対する処理を{}にかく
  to_zero <- p-lim1[1]  #左軸の数値-y1（左軸）の最小値
  y1_range <- lim1[2]-lim1[1] 
  y2_range <- lim2[2]-lim2[1]   # variable_scalerの同じ表記のところと同じ意味
  scaled <- to_zero*y2_range/y1_range   #pは、左軸情報として最小〜最大値があるので、それぞれにy2幅/y1幅をかけることで、左軸情報が右軸情報に変える。
  from_zero <- scaled + lim2[1]   #右軸の最小値分足しておく。
  return(from_zero)}  #3   この関数を使えば、数値の修正が簡単。

ggplot()+geom_line(data=temp_pressure_kumamoto2020, 
                   aes(x=Day, y=Temperature, color="Temperature")) +
  geom_line(data=temp_pressure_kumamoto2020, aes(x=Day, 
                                                 y=variable_scaler(Atmospheric_pressure, 
                                                                   y1.lim, y2.lim), color="Atmospheric_pressure")) + 
  scale_y_continuous(limit=y1.lim, breaks=c(0,10,20,30,40), sec.axis=sec_axis(~(azis_scaler(., y1.lim, y2.lim)), 
                                                                              breaks=c(850,900,950,1000,1050), name="Atmospheric_pressure") )+
  theme_bw()+theme(text=element_text(size=14),legend.position = c(0.98, 0.98),legend.justification = c(0.98, 0.98), legend.background=element_rect(fill="white",color="black"))+scale_x_date(breaks = seq(as.Date("2020-01-01"), as.Date("2020-12-31"), by="2 month"),labels=date_format("%Y/%m")) #4

variable_scaler(US_master$gini)

#2軸グラフ完成

y1.lim <- c(-12, -9) #左軸 temperature
y2.lim <- c(0.3, 0.5) #右軸 Atmospheric_pressure

variable_scaler<- function(p, lim1, lim2){    #pとlim1とlim2に対する処理を{}にかく
  to_zero <- p-lim2[1]     #lim2[1]は、軸スケールの最小値。#4でlim2は、y2.lim（右軸）であり、y2.limの最小値は860。pは気圧の各数値。
  y1_range <- lim1[2]-lim1[1]     #lim1[2]はy1.limの最大値から、lim1[1]はy1.limの最小値を引いたものがy1.lim（左軸）幅ということ。
  y2_range <- lim2[2]-lim2[1]     #上記と同様y2.lim（右軸）の幅
  scaled <- to_zero*y1_range/y2_range    # 気圧の各数値から、最小値860をひく。これに、y1幅/y2幅をかける。y2（右軸）の幅表示をy1（左軸）の幅表示に合わせる作業。
  from_zero <- scaled + lim1[1]     # 左軸の最小値をゼロとするため、もし0以上であれば、その分y1の最小値を足しておく。
  return(from_zero)}   #2 この関数を使えば、数値の修正が簡単。

axis_scaler <- function(p, lim1, lim2){   #pは#4でいうところの.になる。.は左軸情報を表す。左軸とlim1とlim2に対する処理を{}にかく
  to_zero <- p-lim1[1]  #左軸の数値-y1（左軸）の最小値
  y1_range <- lim1[2]-lim1[1] 
  y2_range <- lim2[2]-lim2[1]   # variable_scalerの同じ表記のところと同じ意味
  scaled <- to_zero*y2_range/y1_range   #pは、左軸情報として最小〜最大値があるので、それぞれにy2幅/y1幅をかけることで、左軸情報が右軸情報に変える。
  from_zero <- scaled + lim2[1]   #右軸の最小値分足しておく。
  return(from_zero)}  #3   この関数を使えば、数値の修正が簡単。

  ggplot2::ggplot()+
  ggplot2::geom_line(data=US_master,aes(x=year,y=lgdp_per_capita,color="lgdp per capita"),size=1)+
  ggplot2::geom_line(data=US_master,aes(x=year,y=variable_scaler(gini,y1.lim, y2.lim),color="gini"),size=1)+
  scale_y_continuous( sec.axis=sec_axis(~(axis_scaler(., y1.lim, y2.lim)),name="gini"))+
    labs(title="【US】gini　lgdp_per_capita",subtitle="time series")+
    theme(
      plot.title    = element_text(face = "bold", color = "blue"), 
      axis.title.x  = element_text(size = 20), 
      axis.title.y  = element_text(size = 20), 
      plot.subtitle = element_text(color = "blue", size = 12),
    )+
    theme_bw()
  
  ggplot2::ggplot()+
    ggplot2::geom_line(data=JPN_master,aes(x=year,y=lgdp_per_capita,color="lgdp per capita"),size=1)+
    ggplot2::geom_line(data=JPN_master,aes(x=year,y=variable_scaler(gini,y1.lim, y2.lim),color="gini"),size=1)+
    scale_y_continuous( sec.axis=sec_axis(~(axis_scaler(., y1.lim, y2.lim)),name="gini"))+
    labs(title="【JPN】gini　lgdp_per_capita",subtitle="time series")+
    theme(
      plot.title    = element_text(face = "bold", color = "blue"), 
      axis.title.x  = element_text(size = 20), 
      axis.title.y  = element_text(size = 20), 
      plot.subtitle = element_text(color = "blue", size = 12))+  scale_x_continuous(breaks = seq(JPN_master$year[1],JPN_master$year[length(JPN_master$year)],5))+
    theme_bw()





#gini 0.3~0.5
US_master%>%
  ggplot2::ggplot()+
  ggplot2::geom_line(aes(x=year,y=gini),size=1)+
  ggplot2::geom_line(aes(x=year,y=lgdp_per_capita),size=1)+
  ggplot2::geom_point(aes(x=year,y=gini))+
  labs(title="【US】gini,lgdp_per_capita",subtitle="time series",y="")+
  theme(
    plot.title    = element_text(face = "bold", color = "blue"), 
    axis.title.x  = element_text(size =   20), 
    axis.title.y  = element_text(size = 20), 
    plot.subtitle = element_text(color = "blue", size = 12),
  )+
  scale_x_continuous(breaks = seq(US_master$year[1],US_master$year[length(US_master$year)],5))

JPN_master%>%
  ggplot2::ggplot(aes(x=year,y=gini))+
  ggplot2::geom_line(size=1)+
  ggplot2::geom_point()+
  labs(title="【JPN】gini",subtitle="time series",y="gini")+
  theme(
    plot.title    = element_text(face = "bold", color = "blue"), 
    axis.title.x  = element_text(size = 20), 
    axis.title.y  = element_text(size = 20), 
    plot.subtitle = element_text(color = "blue", size = 12),
  )+
  scale_x_continuous(breaks = seq(JPN_master$year[1],JPN_master$year[length(JPN_master$year)],5))



#分析
#自己共分散を計算する

JPN_gini_acf<-acf(JPN_master$gini,plot=FALSE)
plot(JPN_gini_acf,main = "【JPN】gini コレログラム")

US_gini_acf<-acf(US_master$gini,plot=FALSE)
plot(US_gini_acf,main = "【US】gini コレログラム")

US_lgdp_acf<-acf(US_master$lgdp_per_capita,plot=FALSE)
plot(US_lgdp_acf,main="【US】lgdp per capita コレログラム")

JPN_lgdp_acf<-acf(JPN_master$lgdp_per_capita,plot=FALSE)
plot(JPN_lgdp_acf,main="【JPN】lgdp per capita コレログラム")

install.packages("tseries")
library(tseries)

#一番基本的なモデル(タイムトレンドを考慮,SE typeもhansenみて指定したい)
library(estimatr)
US_general_reg<-lm_robust(gini~lgdp_per_capita+I(lgdp_per_capita^2)+year,data=US_master)
print(US_general_reg)

JPN_general_reg<-lm_robust(gini~lgdp_per_capita+I(lgdp_per_capita^2)+year,data=JPN_master)
print(JPN_general_reg)
#異種分散性テスト
install.packages("lmtest")
lmtest::bptest(US_general_reg)
lmtest::bptest(JPN_general_reg)

#異種分散性が見られるため、SE_type=HC3で計算する
US_general_reg<-lm_robust(gini~lgdp_per_capita+I(lgdp_per_capita^2)+year,data=US_master,se_type="HC3")
print(US_general_reg)

JPN_general_reg<-lm_robust(gini~lgdp_per_capita+I(lgdp_per_capita^2)+year,data=JPN_master,se_type="HC3")
print(JPN_general_reg)


# US_master$gdp_per_capita
# 
# log(US_master$gdp_per_capita)
# 
# #時系列分析用のマスター
# US_time_master<-US_master%>%
#   mutate(lgini=log(gini),lgdp_per_capita=log(gdp_per_capita))
# US_time_master
# 
# JPN_time_master<-JPN_master%>%
#   mutate(lgini=log(gini),lgdp_per_capita=log(gdp_per_capita))
# JPN_time_master
# #一階差分を用いた、トレンド除去時系列モデル(log２乗項　って共線性？)
# US_time_reg<-lm_robust(lgini~lgdp_per_capita+I(lgdp_per_capita^2)+year,data=US_time_master,se_type="HC3")
# print(US_time_reg)
# 
# JPN_time_reg<-lm_robust(lgini~lgdp_per_capita+I(2*lgdp_per_capita)+year,data=JPN_time_master,se_type="HC3")
# print(JPN_time_reg)
# JPN_master

# diff_gdp_per_capita<-JPN_master$gdp_per_capita-dplyr::lag(JPN_master$gdp_per_capita,1)
# JPN_master$gdp_per_capita
# lag(JPN_master$gdp_per_capita,2)
# JPN_master$gdp_per_capita-dplyr::lag(JPN_master$gdp_per_capita)
# diff_gdp_per_capita

#目的変数がI(1)に従うか検定する
tseries::adf.test(US_master$gini,k=1)
tseries::adf.test(JPN_master$gini,k=1)
JPN_master$gini

#一階さ分用のマスター
difference_JPN_master<-JPN_master%>%
  mutate(diff_lgdp_per_capita=lgdp_per_capita-dplyr::lag(lgdp_per_capita),year=year,diff_gini=gini-lag(gini))
difference_JPN_master<-difference_JPN_master%>%
  slice(-1)

difference_US_master<-US_master%>%
  mutate(diff_lgdp_per_capita=lgdp_per_capita-dplyr::lag(lgdp_per_capita),year=year,diff_gini=gini-lag(gini))
difference_US_master<-difference_US_master%>%
  slice(-1)

difference_JPN_master
difference_US_master
JPN_time_reg<-lm_robust(diff_gini~diff_lgdp_per_capita+I(diff_lgdp_per_capita^2),data=difference_JPN_master,se_type="HC3")
print(JPN_time_reg)

US_time_reg<-lm_robust(diff_gini~diff_lgdp_per_capita+I(diff_lgdp_per_capita^2),data=difference_US_master,se_type="HC3")
print(JPN_time_reg)
print(US_time_reg)

#model_summary
install.packages("modelsummary")
library(modelsummary)
regs <- list("detrend Linear Reg US"=US_general_reg,"time first difference US"=US_time_reg,"detrend Linear Reg JPN"=JPN_general_reg,"time first difference JPN"=JPN_time_reg)

var_nam = c("(Intercept)" = "Constant","lgdp_per_capita" = "log gdp per capita", "I(lgdp_per_capita^2)" = "square  of log gdp per capita ","year"="year",
            "diff_lgdp_per_capita"="Δlog gdp per capita","I(diff_lgdp_per_capita^2)"="Δsquare of log gdp per capita")

msummary(regs,fmt = '%.2f',title="time detrend Linear RegとFD estimator比較",coef_map = var_nam)
msummary(regs,fmt = '%.2f',title="P値 time detrend Linear RegとFD estimator比較",coef_map = var_nam,estimate = "p.value")



# #差分における時間はどのようにモデルに入れる？時間を差分取ると多重共線性

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

