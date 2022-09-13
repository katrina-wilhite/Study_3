library(regmed)
library(dplyr)
library(haven)
source("./load_data_by_sex.R")
source("./lasso_munging.R")

munge_lasso("male")
munge_lasso("female")

#Make lambda grid to be used in function to find best fit model 
lambda_grid <- seq(from = 0.4, to = 0.01, by = -0.01)
#Run loop to prefilter mediators and fit to model 



female_outcomes = as.matrix(df_female_outcomes)
y <- female_outcomes
x <- female_exposure
med <- female_lasso_mediators
fit_grid_female <- mvregmed.grid(x, med, y, lambda_grid)

male_outcomes = as.matrix(df_male_outcomes)
y1 <- female_outcomes
x1 <- female_exposure
med1 <- female_lasso_mediators
fit_grid_male <- mvregmed.grid(x1, med1, y1, lambda_grid)