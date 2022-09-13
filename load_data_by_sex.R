library(haven)

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


