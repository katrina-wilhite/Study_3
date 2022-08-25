#install.packages("regmed")
library(regmed)
library(dplyr)
library(haven)

#Load relevant data 
load(file = "C:/Users/katri/Documents/ACU/Study_2/Study_2/Study_2/df_males_domsp_weekday.RData")

lsac_wave6 <- read_sas("Z:/LSAC dataset/General Release/Survey data/SAS/lsacgrb10.sas7bdat")

lsac_wave8 <- read_sas("Z:/LSAC dataset/General Release/Survey data/SAS/lsacgrb14.sas7bdat")

#Select relevanat columns
relevant_wave6_data <- lsac_wave6 %>% 
  select(hicid, fcnfsad2:fcnfseo2d)

relevant_wave8_data <- lsac_wave8 %>% 
  select(hicid, hapsoc:hasdqtb)

#Join datasets
inner_join(df_male_domsp_weekday, relevant_wave6_data, by = "hicid") -> domsp_wave6

inner_join(domsp_wave6, relevant_wave8_data, by = "hicid") -> df_males

#Select mediator variables (domain-specific movement behaviours)
df_males %>% 
  select(active_transport_at_10:nighttime_sleep_at_10) -> mediator_variables

#Structure mediator variables to be a 2-dimensional matrix 
mediator_list=as.list(mediator_variables)

matrix(unlist(mediator_list), ncol = 13, byrow = FALSE) -> mediator_matrix
colnames(mediator_matrix) <- c("1_active_transport", "2_naps", "3_education", "4_leisure_SB", "5_passive_transport", "6_screen_time", "7_self_care", "8_social", "9_sports", "10_unst_LPA", "11_unst_MVPA", "12_household", "13_sleep")
df_mediators <- as.data.frame(mediator_matrix)
#Scale and center mediators
scale(df_mediators, scale = FALSE) -> df_mediators
#Center mediators


interactions_2 <- t(apply(df_mediators, 1, combn, 2, prod))
colnames(df_mediators) <- paste(combn(1:13, 2, paste, collapse="V"), sep = "_")

mediator_interactions_2 <- lapply(1:(ncol(combn(1:ncol(df_mediators), m = 2))),
                                function(y) df_mediators[, combn(1:ncol(df_mediators), m = 2)[,y]])
str(mediator_interactions_2)

#Ensure structure of "mediator_matrix" is the same as "med' in the regmed vignette 
##Run first part of regmed vignette to get data 
data(medsim)
dat.filter <- regmed.prefilter(x[, 1], med, y[, 1], k = 10)
str(dat.filter$med)
##Check structure of "mediator_matrix"
str(mediator_matrix)
#Structure matches so the mediation variable should be set 

#Select exposure variables (SEP)
df_males %>% 
  select(SEP) -> exposure_variable
str(dat.filter$x)
str(exposure_variable)
exposure_variable$SEP -> exposure_SEP_only
str(exposure_SEP_only)
as.matrix(exposure_SEP_only)
str(exposure_SEP_only)
#Does not match dat.filter$x structure
as.matrix(exposure_variable) -> exposure_matrix 
str(exposure_matrix)
#Does not match structure
as.array(exposure_matrix) -> exposure_array
str(exposure_array)
#Does not match structure
exposure_list=as.list(exposure_variable)
str(exposure_list)
#Does not match structure
t(exposure_matrix) -> transposed_exposure_matrix
str(transposed_exposure_matrix)
unname(transposed_exposure_matrix) -> transposed_exposure_matrix2
str(transposed_exposure_matrix2)
dim(transposed_exposure_matrix2) <- c(1,1025)
str(transposed_exposure_matrix2)
attributes(transposed_exposure_matrix2)
dim(transposed_exposure_matrix2)
transposed_exposure_matrix2[c(1,1)]
transposed_exposure_matrix2[,2]
transposed_exposure_matrix2[c(1,2)]


help(regmed)

#Select outcome variables (SDQ results)
df_males %>% 
  select(fcnfsad2) -> outcome_variable
data.matrix(outcome_variable)
outcome_variable$fcnfsad2 -> outcome_SDQ_only
as.matrix(outcome_SDQ_only)
str(outcome_SDQ_only)
as.matrix(outcome_variable) -> outcome_matrix
str(outcome_variable)
str(outcome_matrix)
head(outcome_matrix)
t(outcome_matrix) -> transposed_outcome_matrix
str(transposed_outcome_matrix)
unname(transposed_outcome_matrix) -> transposed_outcome_matrix2
str(transposed_outcome_matrix2)
#Hizzah! 

#Combine new exposure, mediator, and outcome variables into a list 
male_lasso_data <- list(transposed_exposure_matrix2, mediator_matrix, transposed_outcome_matrix2)
str(male_lasso_data)

male_lasso_2 <- list(exposure_SEP_only, mediator_matrix, outcome_SDQ_only)
str(male_lasso_2)
names(male_lasso_2) <- c("exposure", "mediator", "outcome")
str(male_lasso_2)
x <- male_lasso_2$exposure
y <- male_lasso_2$outcome
med <- male_lasso_2$mediator
#Check to ensure the strucutre is the same as the regmed vignette 
str(dat.filter)
str(male_lasso_data)
#Yay! 
names(male_lasso_data) <- c("exposure", "mediator", "outcome")
str(male_lasso_data$mediator)
x <- male_lasso_data$exposure
y <- male_lasso_data$outcome
med <- male_lasso_data$mediator

lambda.grid <- seq(from = 0.4, to = 0.01, by = -0.01)

fit.male <- regmed.grid(x, med, y, lambda.grid, frac.lasso = 0.8)

plot.regmed.grid(fit.male)

fit.best <- regmed.grid.bestfit(fit.male)
summary(fit.best)

edges.med <- regmed.edges(fit.best, type = "mediators")
plot.regmed.edges(edges.med)
#no mediators with individual behaviours 

edges.any <- regmed.edges(fit.best, type = "any")
plot.regmed.edges(edges.any)
#socioeconomic position is related to recreational screen time and structured moderate-vigorous physical activity 