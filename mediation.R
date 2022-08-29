#install.packages("regmed")
library(regmed)
library(dplyr)
library(haven)

#Load relevant data 
load(file = "Z:/LSAC dataset/Study_2/Study_2/df_males_domsp_weekday.RData")

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
df_males <- na.omit(df_males)

#Select mediator variables (domain-specific movement behaviours)
df_males %>% 
  select(active_transport_at_10:nighttime_sleep_at_10) -> mediator_variables

#Structure mediator variables to be a 2-dimensional matrix 
mediator_list=as.list(mediator_variables)

matrix(unlist(mediator_list), ncol = 13, byrow = FALSE) -> mediator_matrix
colnames(mediator_matrix) <- c("v1_active_transport", "v2_naps", "v3_education", "v4_leisure_SB", "v5_passive_transport", "v6_screen_time", "v7_self_care", "v8_social", "v9_sports", "v10_unst_LPA", "v11_unst_MVPA", "v12_household", "v13_sleep")
df_mediators <- as.data.frame(mediator_matrix)
#Scale and center mediators - scale function centers by default 
scale(df_mediators) -> df_mediators
#Center mediators
str(df_mediators)


for (x in 2:12){
  assign(paste0("interactions_", x), (t(apply(df_mediators, 1, combn, x, prod))))
  interaction_number <- get(paste0("interactions_",x))
  colnames(interaction_number) <- paste(combn(1:13, x, paste, collapse="V"), sep = "_")
  assign(paste0("interactions_", x), interaction_number)
}

interactions_13 <- df_mediators[,1]*df_mediators[,2]*df_mediators[,3]*df_mediators[,4]*df_mediators[,5]*df_mediators[,6]*df_mediators[,7]*df_mediators[,8]*df_mediators[,9]*df_mediators[,10]*df_mediators[,11]*df_mediators[,12]*df_mediators[,13]
interactions_13 <- as.matrix(interactions_13)
colnames(interactions_13) <- "1V2V3V4V5V6V7V8V9V10V11V12V13"

cbind(interactions_2, interactions_3, interactions_4, interactions_5, interactions_6, interactions_7, interactions_8, interactions_9, interactions_10, interactions_11, interactions_12, interactions_13) -> mediators_interactions
#Ensure structure of "mediator_matrix" is the same as "med' in the regmed vignette 
##Run first part of regmed vignette to see data 
#data(medsim)
#dat.filter <- regmed.prefilter(x[, 1], med, y[, 1], k = 10)
#str(dat.filter$med)
##Check structure of "mediator_matrix"
#str(mediators_interactions)
#Structure matches so the mediation variable should be set 

#Select exposure variables (SEP)
df_males %>% 
  select(SEP) -> exposure_variable
str(dat.filter$x)
str(exposure_variable)
exposure_variable$SEP -> exposure_variable
str(exposure_variable)


df_males[, 124:129]
for (x in 124:ncol(df_males)) {
  colnames(df_males[,x]) -> name
  assign(paste(name), df_males %>% 
    select(x) %>%
    scale())
 assign(paste0(name),get(name)[,1])
}

#str(hapsoc)
#str(hapeer)
#Wooo! 


#male_lasso_prosocial <- list(exposure_variable, mediators_interactions, hapsoc)
#names(male_lasso_prosocial) <- c("exposure", "mediator", "outcome")
#x <- male_lasso_prosocial$exposure
#y <- male_lasso_prosocial$outcome
#med <- male_lasso_prosocial$mediator
#str(male_lasso_prosocial)
#Check to ensure the structure is the same as the regmed vignette 

cbind(hapsoc, hahypr, haemot, hapeer,  hacondb, hasdqtb) -> df_outcomes
class(df_outcomes)
df_outcomes <- as.data.frame(df_outcomes)
class(df_outcomes)

lambda.grid <- seq(from = 0.4, to = 0.01, by = -0.01)

for(i in 1:ncol(df_outcomes)) {
  colnames(df_outcomes)[i] -> outcome
  assign(paste0("male_lasso_",outcome), list(exposure_variable, mediators_interactions, df_outcomes[,i])) -> outcome_list
  names(outcome_list) <- c("exposure", "mediator", "outcome")
  x <- outcome_list$exposure
  y <- outcome_list$outcome
  med <- outcome_list$mediator
  assign(paste0("dat.filter.", outcome), regmed.prefilter(x, med, y)) -> dat.filter
  x1 <- dat.filter$x
  y1 <- dat.filter$y
  med <- dat.filter$mediator
}



assign(paste0("fit.male.", outcome), regmed.grid(x1, med, y1, lambda.grid, frac.lasso = 0.8)) 

str(male_lasso_hapsoc$outcome)

data(male_lasso_prosocial)
dat.filter.prosocial <- regmed.prefilter(x, med, y)



x1 <- dat.filter.prosocial$x
y1 <- dat.filter.prosocial$y
med <- dat.filter.prosocial$mediator

fit.male.prosocial <- regmed.grid(x1, med, y1, lambda.grid, frac.lasso = 0.8)

plot.regmed.grid(fit.male.prosocial)

fit.best.prosocial <- regmed.grid.bestfit(fit.male.prosocial)
summary(fit.best.prosocial)

fit.single <- regmed.fit(x1, med, y1, lambda = .12, frac.lasso =0.8)
summary(fit.single)

edges.med.prosocial <- regmed.edges(fit.single, type = "mediators")
plot.regmed.edges(edges.med.prosocial)

edges.med <- regmed.edges(fit.best.prosocial, type = "mediators")
class(edges.med)
plot.regmed.edges(edges.med)
class(fit.best.prosocial)
#no mediators with individual behaviours 

edges.any <- regmed.edges(fit.best.prosocial, type = "any")
plot.regmed.edges(edges.any)

edges.any <- regmed.edges(fit.single, type = "any")
plot.regmed.edges(edges.any)



#Repeat above for other outcome variables 
male_lasso_hyperactivity <- list(exposure_variable, mediators_interactions, hahypr)
names(male_lasso_hyperactivity) <- c("exposure", "mediator", "outcome")
x <- male_lasso_hyperactivity$exposure
y <- male_lasso_hyperactivity$outcome
med <- male_lasso_hyperactivity$mediator

data(male_lasso_prosocial)
dat.filter.prosocial <- regmed.prefilter(x, med, y)

lambda.grid <- seq(from = 0.4, to = 0.01, by = -0.01)

x1 <- dat.filter.prosocial$x
y1 <- dat.filter.prosocial$y
med <- dat.filter.prosocial$mediator

fit.male.prosocial <- regmed.grid(x1, med, y1, lambda.grid, frac.lasso = 0.8)