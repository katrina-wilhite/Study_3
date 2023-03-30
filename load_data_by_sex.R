library(haven)
install.packages("cloudstoR")
library("cloudstoR")
cloud_auth(reset_keys = TRUE)

cloud_get(path = "Shared/LSAC - Katrina/Study_2/Study_2/df_males_domsp_weekday.RData")
cloud_list(path = 'cloudstoR')

male_data <- function(){
  load(file = "Z:/LSAC dataset/Study_2/Study_2/df_males_domsp_weekday.RData", envir = globalenv())
  .GlobalEnv$lsac_wave6 <- read_sas("Z:/LSAC dataset/General Release/Survey data/SAS/lsacgrb10.sas7bdat")
  .GlobalEnv$lsac_wave8 <- read_sas("Z:/LSAC dataset/General Release/Survey data/SAS/lsacgrb14.sas7bdat")
}


female_data <- function(){
  load(file = "Z:/LSAC dataset/Study_2/Study_2/df_females_domsp_weekday.RData", envir = globalenv())
  .GlobalEnv$lsac_wave6 <- read_sas("Z:/LSAC dataset/General Release/Survey data/SAS/lsacgrb10.sas7bdat")
  .GlobalEnv$lsac_wave8 <- read_sas("Z:/LSAC dataset/General Release/Survey data/SAS/lsacgrb14.sas7bdat")
}

cloud_get(path = "Shared/LSAC - Katrina/Study_2/Study_2/df_females_domsp_weekday.RData")
#Updated code retrieved from cloudstoR
male_data <- function(){
  cloud_get(path = "Shared/LSAC - Katrina/Study_2/Study_2/df_males_domsp_weekday.RData")
  .GlobalEnv$lsac_wave6 <- read_sas("Z:/LSAC dataset/General Release/Survey data/SAS/lsacgrb10.sas7bdat")
  .GlobalEnv$lsac_wave8 <- read_sas("Z:/LSAC dataset/General Release/Survey data/SAS/lsacgrb14.sas7bdat")
}


female_data <- function(){
  cloud_get(path = "Shared/LSAC - Katrina/Study_2/Study_2/df_females_domsp_weekday.RData")
  .GlobalEnv$lsac_wave6 <- read_sas("Z:/LSAC dataset/General Release/Survey data/SAS/lsacgrb10.sas7bdat")
  .GlobalEnv$lsac_wave8 <- read_sas("Z:/LSAC dataset/General Release/Survey data/SAS/lsacgrb14.sas7bdat")
}