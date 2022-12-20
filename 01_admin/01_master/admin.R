source("01_admin/02_preamble/R/admin.R")

library_list<-c(
  "tidyverse",
  "tidyr",
  "dplyr",
  "zoo",
  "ggplot2",
  "estimatr",
  "lmtest",
  "modelsummary",
  "kableExtra",
  "tseries"
  
)
install.packages(library_list)
library(tidyverse)
library(dplyr)
library(tidyr)
library(zoo)
library(modelsummary)
library(ggplot2)
library(estimatr)
library(lmtest)
library(kableExtra)
library(tseries)

#-------
source("03_build/gdp_tidy/code/build.R")

source("03_build/inequality_tidy/code/build.R")   

source("03_build/gdp_ready/code/build.R")

source("03_build/inequality_ready/code/build.R")   

source("03_build/master/code/build.R")  

source("04_analyze/initial/code/analyze.R")  

