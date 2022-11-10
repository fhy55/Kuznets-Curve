# #long_gini_data
# long_gini_data
# #データがない年を線形で補填する
# us_first_observation_year <- 1974
# us_last_observation_year<-2019
# japan_first_observation_year <- 1990
# japan_last_observation_year<-2017
# us_data_year_range <- us_last_observation_year - us_first_observation_year
# japan_data_year_range <- japan_last_observation_year - japan_first_observation_year
# missing_year <- c()
# for(i in 1:us_data_year_range){
#   year <- us_first_observation_year + i
#   if(long_gini_data$country=="USA"&&long_gini_data$year[i] !=year){
#     long_gini_data$year <- year
#   }
# }
# 26+17
# 
# #普通にginiが無ければ前後で補填すればよい→線形だから、5年飛んでたらそのギャップに合わせて変換する
# year_number <- unique(long_gini_data$year)
# year_number
# long_gini_data$gini
nrow_gini<- nrow(long_gini_data)
nrow_gini
# is.na(long_gini_data$gini[3])
# long_gini_data$year[3]
# 
# before_gini <- long_gini_data$gini[2]
# after_gini <- long_gini_data$gini[4]
# before_year <- long_gini_data$year[2]
# after_year <- long_gini_data$year[4]
# change_rate <- (after_gini-before_gini)/(after_year-before_year)
# change_year<- long_gini_data$year[3]-long_gini_data$year[2]
# long_gini_data$gini[3]<-before_gini+change_rate*change_year
# long_gini_data$gini[3]
# for(i in 1:nrow_gini){
#   if(is.na(long_gini_data$gini[i])){
#     print(long_gini_data$gini[i])
#     print(long_gini_data$year[i])
#   }
# }
# 
# long_gini_data


#間のない年を補填する.取り扱いやすくするためにUSと日本で分ける
japan_gini <- subset(long_gini_data,long_gini_data$country=="JPN")
japan_gini
us_gini <- subset(long_gini_data,long_gini_data$country=="USA")
us_gini

us_gini<-complete(us_gini, year = full_seq(year, 1))
japan_gini<-complete(japan_gini, year = full_seq(year, 1))
us_gini$country<-"USA"
us_gini
japan_gini$country<-"JPN"
japan_gini

install.packages("zoo")
library(zoo)
japan_gini$gini<-na.approx(japan_gini$gini)


us_gini$gini<-na.approx(us_gini$gini)


japan_gini_multiple_5 <- japan_gini%>%
  filter(year %% 5==0)
japan_gini_multiple_5

us_gini_multiple_5 <- us_gini%>%
  filter(year %% 5==0)
us_gini_multiple_5

#5年ずつ抽出する

convine_japan_us_gini <- us_gini_multiple_5%>%
  full_join(japan_gini_multiple_5)
# View(convine_japan_us_gini)
#------ここまでが完成したところ

# #NAを線形で補填する(これは日本アメリカ別々の時にやるべき)
# #別々にやるならnrow_giniも変更する
# for(i in 1:nrow_gini){
#   if(is.na(long_gini_data$gini[i])){
#     before_gini <- long_gini_data$gini[i-1]
#     print(paste("before_gini",before_gini))
#     cnt <- i+1
#     while(is.na(long_gini_data$gini[cnt]){
#       #naが続く限りループする.naになっていない場所を探したい
#     }
#     if(is.na(long_gini_data$gini[i+1])){
#       #ここを変更する.ここでNAが連続の場合を考えている
#       #どこまで拡張性を持たせるべきか、拡張性持たせるならNAが永遠につながる場合もやりたい
#       after_gini <- long_gini_data$gini[i+2]
#     }
#     print(paste("after_gini",after_gini))
#     before_year <- long_gini_data$year[i-1]
#     print(paste("before_year",before_year))
#     after_year <- long_gini_data$year[i+1]
#     print(paste("after_year",after_year))
#     change_rate <- (after_gini-before_gini)/(after_year-before_year)
#     print(paste("after-before_gini",after_gini-before_gini))
#     print(paste("after-before_year",after_year-before_year))
#     change_year<- long_gini_data$year[i]-long_gini_data$year[i-1]
#     print(change_rate*change_year)
#     print(change_year)
#     print(change_rate)
#     long_gini_data$gini[i] <- before_gini + change_rate*change_year
#   }
# }
# 
# #二年連続のNAに対応できていなかった、どこまで拡張性を満たせばよいか
# long_gini_data$gini
# long_gini_data
# #次のデータの間があればその差分のデータを追加する
# #疑問は、giniが欠けているところのみを線形補填すればいいのか。そもそも年度がないところも補填すべきなのか→たぶん全部補填すべき
# #まず年度を全て補填する
# #その上で連続NAにも対応するように上のものを書き換える
# supplement_year_gini_data <- 
# 
# 
# 
# # 5年おきのデータを抽出する,なぜ→理由分ってからやる→gdpのデータが5年毎だから、どのみち結どうするなら気にする必要ないのでは
# us_first_observation_year <- 1974
# us_last_observation_year<-2019
# japan_first_observation_year <- 1990
# japan_last_observation_year<-2017
# us_data_year_range <- us_last_observation_year - us_first_observation_year
# japan_data_year_range <- japan_last_observation_year - japan_first_observation_year
# 
# long_gini_data
# for(i in 1:us_d)
#   year <- us_first_observation_year + i
#   if(long_gini_data$country=="USA"){
#     if(long_gini_data$year[i] !=year){
#       
#       long_gini_data$year <- year
#   }
#   }
# long_gini_data$year[0]
# 
# if(long_gini_data$contry[1]=="USA"){
#   if(long_gini_data$year[1] != (long_gini_data$year[1-1]+1)){
#     add_data <- c("USA",long_gini_data$year[1]+1,NA)
#     long_gini_data<- rbind(long_gini_data,add_data)
#   }}
# 
# long_gini_data
# 
# 
# nrow_gini<- nrow(long_gini_data)
# nrow_gini
# for(i in 1:nrow_gini){
#   if(long_gini_data$contry[i]=="USA"){
#     if(long_gini_data$year[i]==2019) next #最後の年はnext
#     if(long_gini_data$year[i] != (long_gini_data$year[i-1]+1)){
#       add_data <- c("USA",long_gini_data$year[i]+1,NA)
#         long_gini_data<- rbind(long_gini_data,add_data)
#       }
#   }
#   if(long_gini_data$contry[i]=="japan"){
#     if(long_gini_data$year[i]==2017) next
#     if(long_gini_data$year[i] != (long_gini_data$year[i-1]+1)){
#       add_data <- c("USA",long_gini_data$year[i]+1,NA)
#       long_gini_data<- rbind(long_gini_data,add_data)
#     }
#   }
# 
# }


#

