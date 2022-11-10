main <- function(){
  box::use(`functions`/basics)

}

#記述統計の作成
#記述統計は、分析後に行う？
#分析上、日本とアメリカに分ける

View(master_data)
install.packages("skimr")
library(skimr)
descriptive_statistics<-skimr::skim(master_data)
View(descriptive_statistics)
main()