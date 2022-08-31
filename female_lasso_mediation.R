#install.packages("regmed")
library(regmed)
library(dplyr)
library(haven)

#Load relevant data 
load(file = "Z:/LSAC dataset/Study_2/Study_2/df_female_domsp_weekday.Rdata") -> df_female_domsp_weekday

lsac_wave6 <- read_sas("Z:/LSAC dataset/General Release/Survey data/SAS/lsacgrb10.sas7bdat")

lsac_wave8 <- read_sas("Z:/LSAC dataset/General Release/Survey data/SAS/lsacgrb14.sas7bdat")

#Select relevanat columns
relevant_wave6_data <- lsac_wave6 %>% 
  select(hicid, fcnfsad2:fcnfseo2d)

relevant_wave8_data <- lsac_wave8 %>% 
  select(hicid, hapsoc:hasdqtb)

#Join datasets
inner_join(df_female_domsp_weekday, relevant_wave6_data, by = "hicid") -> domsp_wave6

inner_join(domsp_wave6, relevant_wave8_data, by = "hicid") -> df_females
df_females <- na.omit(df_females)

#Select mediator variables (domain-specific movement behaviours)
df_females %>% 
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
df_females %>% 
  select(SEP) -> exposure_variable
str(dat.filter$x)
str(exposure_variable)
exposure_variable$SEP -> exposure_variable
str(exposure_variable)


for (x in 123:ncol(df_females)) {
  colnames(df_females[,x]) -> name
  assign(paste(name), df_females %>% 
           select(x) %>%
           scale())
  assign(paste0(name),get(name)[,1])
}


str(hapsoc)
#str(hapeer)
#Wooo! 


cbind(hapsoc, hahypr, haemot, hapeer,  hacondb, hasdqtb) -> df_outcomes
class(df_outcomes)
df_outcomes <- as.data.frame(df_outcomes)
class(df_outcomes)

lambda.grid <- seq(from = 0.4, to = 0.01, by = -0.01)

for(i in 1:ncol(df_outcomes)) {
  colnames(df_outcomes)[i] -> outcome
  assign(paste0("female_lasso_",outcome), list(exposure_variable, mediators_interactions, df_outcomes[,i])) -> outcome_list
  names(outcome_list) <- c("exposure", "mediator", "outcome")
  x <- outcome_list$exposure
  y <- outcome_list$outcome
  med <- outcome_list$mediator
  assign(paste0("dat.filter.", outcome), regmed.prefilter(x, med, y, k = 10)) -> dat.filter
  x1 <- dat.filter$x
  y1 <- dat.filter$y
  med <- dat.filter$mediator
  assign(paste0("fit.female.", outcome), regmed.grid(x1, med, y1, lambda.grid, frac.lasso = 0.8)) 
}

#Find hapsoc mediators 
plot.regmed.grid(fit.female.hapsoc)
fit.best.hapsoc <- regmed.grid.bestfit(fit.female.hapsoc)
summary(fit.best.hapsoc)

edges.med.hapsoc <- regmed.edges(fit.best.hapsoc, type = "mediators")
plot.regmed.edges(edges.med.hapsoc)

edges.any.hapsoc <- regmed.edges(fit.best.hapsoc, type = "any")
plot.regmed.edges(edges.any.hapsoc)

fit.single.hapsoc <- regmed.fit(x1, med, y1, lambda = .12, frac.lasso =0.8)
summary(fit.single.hapsoc)

edges.med.hapsoc.single <- regmed.edges(fit.single.hapsoc, type = "mediators")
plot.regmed.edges(edges.med.hapsoc.single)

edges.any.hapsoc.single <- regmed.edges(fit.single.hapsoc, type = "any")
plot.regmed.edges(edges.any.hapsoc.single, v.size = 40, x.color = "yellow", y.color = "green", vertex.label.color = "blue" )


#Repeat for hyperactivity 
plot.regmed.grid(fit.female.hahypr)
fit.best.hahypr <- regmed.grid.bestfit(fit.female.hahypr)
summary(fit.best.hahypr)

edges.med.hahypr <- regmed.edges(fit.best.hahypr, type = "mediators")
plot.regmed.edges(edges.med.hahypr)

edges.any.hahypr <- regmed.edges(fit.best.hahypr, type = "any")
plot.regmed.edges(edges.any)

fit.single.hahypr <- regmed.fit(x1, med, y1, lambda = .19, frac.lasso =0.8)
summary(fit.single.hahypr)

edges.med.hahypr.single <- regmed.edges(fit.single.hahypr, type = "mediators")
plot.regmed.edges(edges.med.hahypr.single)

edges.any.hahypr.single <- regmed.edges(fit.single.hahypr, type = "any")
plot.regmed.edges(edges.any.hahypr.single)

#Repeat for emotional problems 
plot.regmed.grid(fit.female.haemot)
fit.best.haemot <- regmed.grid.bestfit(fit.female.haemot)
summary(fit.best.haemot)

edges.med.haemot <- regmed.edges(fit.best.haemot, type = "mediators")
plot.regmed.edges(edges.med.haemot)

edges.any.haemot <- regmed.edges(fit.best.haemot, type = "any")
plot.regmed.edges(edges.any)

fit.single.haemot <- regmed.fit(x1, med, y1, lambda = .14, frac.lasso =0.8)
summary(fit.single.haemot)

edges.med.haemot.single <- regmed.edges(fit.single.haemot, type = "mediators")
plot.regmed.edges(edges.med.haemot.single)

edges.any.haemot.single <- regmed.edges(fit.single.haemot, type = "any")
plot.regmed.edges(edges.any.haemot.single)

#Repeat for peer problems 
plot.regmed.grid(fit.female.hapeer)
fit.best.hapeer <- regmed.grid.bestfit(fit.female.hapeer)
summary(fit.best.hapeer)

edges.med.hapeer <- regmed.edges(fit.best.hapeer, type = "mediators")
plot.regmed.edges(edges.med.hapeer)

edges.any.hapeer <- regmed.edges(fit.best.hapeer, type = "any")
plot.regmed.edges(edges.any)

fit.single.hapeer <- regmed.fit(x1, med, y1, lambda = .15, frac.lasso =0.8)
summary(fit.single.hapeer)

edges.med.hapeer.single <- regmed.edges(fit.single.hapeer, type = "mediators")
plot.regmed.edges(edges.med.hapeer.single)

edges.any.hapeer.single <- regmed.edges(fit.single.hapeer, type = "any")
plot.regmed.edges(edges.any.hapeer.single)

#Repeat for conduct problems 
plot.regmed.grid(fit.female.hacondb)
fit.best.hacondb <- regmed.grid.bestfit(fit.female.hacondb)
summary(fit.best.hacondb)

edges.med.hacondb <- regmed.edges(fit.best.hacondb, type = "mediators")
plot.regmed.edges(edges.med.hacondb)

edges.any.hacondb <- regmed.edges(fit.best.hacondb, type = "any")
plot.regmed.edges(edges.any)

fit.single.hacondb <- regmed.fit(x1, med, y1, lambda = .15, frac.lasso =0.8)
summary(fit.single.hacondb)

edges.med.hacondb.single <- regmed.edges(fit.single.hacondb, type = "mediators")
plot.regmed.edges(edges.med.hacondb.single)

edges.any.hacondb.single <- regmed.edges(fit.single.hacondb, type = "any")
plot.regmed.edges(edges.any.hacondb.single)

#Repeat for total socio-emotional problems 
plot.regmed.grid(fit.female.hasdqtb)
fit.best.hasdqtb <- regmed.grid.bestfit(fit.female.hasdqtb)
summary(fit.best.hasdqtb)

edges.med.hasdqtb <- regmed.edges(fit.best.hasdqtb, type = "mediators")
plot.regmed.edges(edges.med.hasdqtb)

edges.any.hasdqtb <- regmed.edges(fit.best.hasdqtb, type = "any")
plot.regmed.edges(edges.any.hasdqtb)

fit.single.hasdqtb <- regmed.fit(x1, med, y1, lambda = .18, frac.lasso =0.8)
summary(fit.single.hasdqtb)

edges.med.hasdqtb.single <- regmed.edges(fit.single.hasdqtb, type = "mediators")
plot.regmed.edges(edges.med.hasdqtb.single)

edges.any.hasdqtb.single <- regmed.edges(fit.single.hasdqtb, type = "any")
plot.regmed.edges(edges.any.hasdqtb.single)


