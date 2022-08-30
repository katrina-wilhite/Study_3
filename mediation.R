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
  assign(paste0("dat.filter.", outcome), regmed.prefilter(x, med, y, k = 10)) -> dat.filter
  x1 <- dat.filter$x
  y1 <- dat.filter$y
  med <- dat.filter$mediator
  assign(paste0("fit.male.", outcome), regmed.grid(x1, med, y1, lambda.grid, frac.lasso = 0.8)) 
}

#Find hapsoc mediators 
plot.regmed.grid(fit.male.hapsoc)
fit.best.hapsoc <- regmed.grid.bestfit(fit.male.hapsoc)
summary(fit.best.hapsoc)

edges.med.hapsoc <- regmed.edges(fit.best.hapsoc, type = "mediators")
plot.regmed.edges(edges.med.hapsoc)

edges.any.hapsoc <- regmed.edges(fit.best.hapsoc, type = "any")
plot.regmed.edges(edges.any)

fit.single.hapsoc <- regmed.fit(x1, med, y1, lambda = .12, frac.lasso =0.8)
summary(fit.single.hapsoc)

edges.med.hapsoc.single <- regmed.edges(fit.single.hapsoc, type = "mediators")
plot.regmed.edges(edges.med.hapsoc.single)

edges.any.hapsoc.single <- regmed.edges(fit.single.hapsoc, type = "any")
plot.regmed.edges(edges.any.hapsoc.single, v.size = 40, x.color = "yellow", y.color = "green", vertex.label.color = "blue" )

install.packages("GGally")
library(GGally)
install.packages("network")
install.packages("sna")
library(network)
library(sna)
library(ggplot2)

ggnet2(edges.med.hapsoc.single)
class(edges.any.hapsoc.single)

prosocial_mediators <- 

#Graph mediation 
make_directed_graph(edges.any.hapsoc.single)

#Repeat for hyperactivity 
plot.regmed.grid(fit.male.hahypr)
fit.best.hahypr <- regmed.grid.bestfit(fit.male.hahypr)
summary(fit.best.hahypr)

edges.med.hahypr <- regmed.edges(fit.best.hahypr, type = "mediators")
plot.regmed.edges(edges.med.hahypr)

edges.any.hahypr <- regmed.edges(fit.best.hahypr, type = "any")
plot.regmed.edges(edges.any)

fit.single.hahypr <- regmed.fit(x1, med, y1, lambda = .15, frac.lasso =0.8)
summary(fit.single.hahypr)

edges.med.hahypr.single <- regmed.edges(fit.single.hahypr, type = "mediators")
plot.regmed.edges(edges.med.hahypr.single)

edges.any.hahypr.single <- regmed.edges(fit.single.hahypr, type = "any")
plot.regmed.edges(edges.any.hahypr.single)

#Repeat for emotional problems 
plot.regmed.grid(fit.male.haemot)
fit.best.haemot <- regmed.grid.bestfit(fit.male.haemot)
summary(fit.best.haemot)

edges.med.haemot <- regmed.edges(fit.best.haemot, type = "mediators")
plot.regmed.edges(edges.med.haemot)

edges.any.haemot <- regmed.edges(fit.best.haemot, type = "any")
plot.regmed.edges(edges.any)

fit.single.haemot <- regmed.fit(x1, med, y1, lambda = .16, frac.lasso =0.8)
summary(fit.single.haemot)

edges.med.haemot.single <- regmed.edges(fit.single.haemot, type = "mediators")
plot.regmed.edges(edges.med.haemot.single)

edges.any.haemot.single <- regmed.edges(fit.single.haemot, type = "any")
plot.regmed.edges(edges.any.haemot.single)

#Repeat for peer problems 
plot.regmed.grid(fit.male.hapeer)
fit.best.hapeer <- regmed.grid.bestfit(fit.male.hapeer)
summary(fit.best.hapeer)

edges.med.hapeer <- regmed.edges(fit.best.hapeer, type = "mediators")
plot.regmed.edges(edges.med.hapeer)

edges.any.hapeer <- regmed.edges(fit.best.hapeer, type = "any")
plot.regmed.edges(edges.any)

fit.single.hapeer <- regmed.fit(x1, med, y1, lambda = .13, frac.lasso =0.8)
summary(fit.single.hapeer)

edges.med.hapeer.single <- regmed.edges(fit.single.hapeer, type = "mediators")
plot.regmed.edges(edges.med.hapeer.single)

edges.any.hapeer.single <- regmed.edges(fit.single.hapeer, type = "any")
plot.regmed.edges(edges.any.hapeer.single)

#Repeat for conduct problems 
plot.regmed.grid(fit.male.hacondb)
fit.best.hacondb <- regmed.grid.bestfit(fit.male.hacondb)
summary(fit.best.hacondb)

edges.med.hacondb <- regmed.edges(fit.best.hacondb, type = "mediators")
plot.regmed.edges(edges.med.hacondb)

edges.any.hacondb <- regmed.edges(fit.best.hacondb, type = "any")
plot.regmed.edges(edges.any)

fit.single.hacondb <- regmed.fit(x1, med, y1, lambda = .18, frac.lasso =0.8)
summary(fit.single.hacondb)

edges.med.hacondb.single <- regmed.edges(fit.single.hacondb, type = "mediators")
plot.regmed.edges(edges.med.hacondb.single)

edges.any.hacondb.single <- regmed.edges(fit.single.hacondb, type = "any")
plot.regmed.edges(edges.any.hacondb.single)

#Repeat for total socio-emotional problems 
plot.regmed.grid(fit.male.hasdqtb)
fit.best.hasdqtb <- regmed.grid.bestfit(fit.male.hasdqtb)
summary(fit.best.hasdqtb)

edges.med.hasdqtb <- regmed.edges(fit.best.hasdqtb, type = "mediators")
plot.regmed.edges(edges.med.hasdqtb)

edges.any.hasdqtb <- regmed.edges(fit.best.hasdqtb, type = "any")
plot.regmed.edges(edges.any)

fit.single.hasdqtb <- regmed.fit(x1, med, y1, lambda = .13, frac.lasso =0.8)
summary(fit.single.hasdqtb)

edges.med.hasdqtb.single <- regmed.edges(fit.single.hasdqtb, type = "mediators")
plot.regmed.edges(edges.med.hasdqtb.single)

edges.any.hasdqtb.single <- regmed.edges(fit.single.hasdqtb, type = "any")
plot.regmed.edges(edges.any.hasdqtb.single)

install.packages("diagram")
library(diagram)
med_data_hapsoc <- 

install.packages("glue")
install.packages("DiagrammeR")
library(glue)
library(DiagrammeR)

med_data_hapsoc1 <- 
  data.frame(
    lab_x = "Socioeconomic position",
    lab_m1 = "Mediator 1", 
    lab_m2 = "Mediator 2", 
    lab_m3 = "Mediator 3", 
    lab_y = "Prosocial behaviour",
    coef_xm1 = ".34",
    coef_xm2 = ".27",
    coef_xm3 = ".54",
    coef_m1y = ".73",
    coef_m2y = ".64", 
    coef_m3y = ".19",
    coef_xy = ".43")



#Code for running one outcome at a time 
#male_lasso_prosocial <- list(exposure_variable, mediators_interactions, hapsoc)
#names(male_lasso_prosocial) <- c("exposure", "mediator", "outcome")
#x <- male_lasso_prosocial$exposure
#y <- male_lasso_prosocial$outcome
#med <- male_lasso_prosocial$mediator
#str(male_lasso_prosocial)
#Check to ensure the structure is the same as the regmed vignette 
#data(male_lasso_prosocial)
#dat.filter.prosocial <- regmed.prefilter(x, med, y)
#x1 <- dat.filter.prosocial$x
#y1 <- dat.filter.prosocial$y
#med <- dat.filter.prosocial$mediator
#fit.male.prosocial <- regmed.grid(x1, med, y1, lambda.grid, frac.lasso = 0.8)
#plot.regmed.grid(fit.male.prosocial)



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