library(haven)

male_data <- function() {
  load(file = "data/df_males_domsp_weekday.RData", envir = globalenv())
  .GlobalEnv$lsac_wave6 <- read_sas("data/lsacgrb10.sas7bdat")
  .GlobalEnv$lsac_wave8 <- read_sas("data/lsacgrb14.sas7bdat")
}


female_data <- function() {
  load(file = "data/df_females_domsp_weekday.RData", envir = globalenv())
  .GlobalEnv$lsac_wave6 <- read_sas("data/lsacgrb10.sas7bdat")
  .GlobalEnv$lsac_wave8 <- read_sas("data/lsacgrb14.sas7bdat")
}
