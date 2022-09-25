library(regmed)
library(dplyr)
library(haven)
source("./load_data_by_sex.R")
source("./lasso_munging.R")

munge_lasso("male")
munge_lasso("female")



#Run loop to prefilter mediators and fit to model 
lambda_grid <- seq(from = 0.4, to = 0.01, by = -0.01)
#Run loop to prefilter mediators and fit to model 
for(i in 1:ncol(df_female_outcomes)) {
  outcome <- colnames(df_female_outcomes)[i]  
  outcome_list <- assign(paste0("female_lasso_",outcome), list(female_exposure, female_lasso_mediators, df_female_outcomes[,i])) 
  names(outcome_list) <- c("exposure", "mediator", "outcome")
  x <- outcome_list$exposure
  y <- outcome_list$outcome
  med <- outcome_list$mediator
  dat_filter <- assign(paste0("dat_filter_", outcome), regmed.prefilter(x, med, y, k = 100)) 
  x1 <- dat_filter$x
  y1 <- dat_filter$y
  med <- dat_filter$mediator
  assign(paste0("fit_female_", outcome), regmed.grid(x1, med, y1, lambda_grid, frac.lasso = 0.8)) 
}


#Run loop to prefilter mediators and fit to model 
for(i in 1:ncol(df_male_outcomes)) {
  outcome <- colnames(df_male_outcomes)[i]  
  outcome_list <- assign(paste0("male_lasso_",outcome), list(male_exposure, male_lasso_mediators, df_male_outcomes[,i])) 
  names(outcome_list) <- c("exposure", "mediator", "outcome")
  x <- outcome_list$exposure
  y <- outcome_list$outcome
  med <- outcome_list$mediator
  dat_filter <- assign(paste0("dat_filter_", outcome), regmed.prefilter(x, med, y, k = 100)) 
  x1 <- dat_filter$x
  y1 <- dat_filter$y
  med <- dat_filter$mediator
  assign(paste0("fit_male_", outcome), regmed.grid(x1, med, y1, lambda_grid, frac.lasso = 0.8)) 
}
