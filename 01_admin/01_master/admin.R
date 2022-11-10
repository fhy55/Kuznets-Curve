main <- function(){
  preamble()
  build()
  analyze()
  report()
  postamble()
}


source("01_admin/02_preamble/R/admin.R")
# 
 # source("01_admin/initialize/admin.R")
# #initializeを呼び出すとrlangのバージョンを戻すことを要求されるが、受け入れると古すぎる→受け入れなくてもよい？
#rlangのバージョンを新しくしたい→r自体を更新したらできた
# renv::update()
# library(rlang)
# package_version("rlang")
# library()


# devtools::install_github("r-lib/rlang", build_vignettes = TRUE)

#packageのフォルダのコード実行したら更新された
# library()
# #自分でlibraryを指定してinstallする
# gc()
# # 
# library_list<-c(
#   "tidyverse",
#   "tidyr",
#   "dplyr",
#   "purrr",
#   "zoo",
#   "devtools",
#   "usethis",
#   "cli"
# )
# install.packages(library_list)
library(tidyverse)
library(dplyr)
library(tidyr)
# library(zoo)
# library(rlang)
# library(devtools)
# devtools::install_github("r-lib/rlang", build_vignettes = TRUE)
# 
# update.packages("rlang")
# package_version("rlang")
# update.packages("cli")
# package_version("cli")
# #全体的にパッケージが古い問題
# devtools::install_github("r-lib/rlang", build_vignettes = TRUE)

.libPaths()
#renvをいじる必要ありそう

#-------
source("03_build/gdp_tidy/code/build.R")

source("03_build/inequality_tidy/code/build.R")   

source("03_build/gdp_ready/code/build.R")

source("03_build/inequality_ready/code/build.R")   

source("03_build/master/code/build.R")  

source("04_analyze/initial/code/analyze.R")  

preamble <- function(){
  lets('set', 'preamble')
}


build <- function(){
  lets("build","master")
}


##追記した
analyze <- function(){
  lets('analyze', 'initial')
}


report <- function(){
  lets('report', 'initial')
}


postamble <- function(){
  lets('set', 'postamble')
}

lets <- function(verb_name, object_name){
  if (verb_name == 'set' && object_name == 'preamble'){
    source(here::here('01_admin', '02_preamble', 'R', 'admin.R'))
  }
  
  else if (verb_name == 'build'){
    source(here::here('03_build', object_name, 'code', 'build.R'))
  }
  
  else if (verb_name == 'analyze'){
    source(here::here('04_analyze', object_name, 'code', 'analyze.R'))
  }
  
  else if (verb_name == 'report'){
    rmarkdown::render(here::here('05_report', 
                                 object_name, 'text', 'report.Rmd'),
                      output_dir = here::here('05_report', 
                                              object_name, 'output')) 
  }
  
  else if (verb_name == 'set' && object_name == 'postamble'){
    source(here::here('01_admin', '03_postamble', 'admin.R'))
  }
  
}

main()
