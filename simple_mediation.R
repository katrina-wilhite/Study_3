#RQ: Does domain-specific movement behaviour profile membership mediate the relationship between socioeconomic position and socio-emotional outcomes in children?
##Simple mediation for males

library(medflex)
library(dplyr)
library(haven)
library(rmarkdown)

source("./load_data_by_sex.R")
source("./munging.R")
#male_data()
#munge("male")
#female_data()
#munge("female")

#Run loop for all socio-emotional variables
for (x in 21:ncol(df_male)) {
  name <- colnames(df_male[,x])
  assign("objectname", (paste(name,"~","SEP + model3_trajectory_assignments + Remoteness + Indigenous + adv + dis + resources + mother_home + father_home + mental_health + mother_race + father_race")))
  expData <- assign(paste0("expData_", name), neImpute(objectname, data = df_male))
  assign("model", (paste(name,"~",("SEP0 + SEP1 + Remoteness + Indigenous + adv + dis + resources + mother_home + father_home + mental_health + mother_race + father_race"))))
  outcome_results <- assign(paste0("ne_", name), neModel(model, expData = expData, se = "robust"))
  assign(paste0("male_effdecomp_", name), envir = globalenv(), neEffdecomp(outcome_results))
}
#Summary of results in "results__male_mediation.Rmd" 

for (x in 20:ncol(df_female)) {
  name <- colnames(df_female[,x])
  assign("objectname", (paste(name,"~","SEP + model4_trajectory_assignments + Remoteness + Indigenous + adv + dis + resources + mother_home + father_home + mental_health + father_race")))
  expData <- assign(paste0("expData_", name), neImpute(objectname, data = df_female))
  assign("model", (paste(name,"~",("SEP0 + SEP1 + Remoteness + Indigenous + adv + dis + resources + mother_home + father_home + mental_health + father_race"))))
  outcome_results <- assign(paste0("ne_", name), neModel(model, expData = expData, se = "robust"))
  assign(paste0("female_effdecomp_", name), envir = globalenv(), neEffdecomp(outcome_results))
}



# 


#See RMD file "simple_mediation_results.rmd" for summaries of all effdecomp objects 



#I tried to make a function but I couldn't get it to work! I couldn't figure it out but it kept returning erros out on the last line. No clue why.  
# simple_mediation <- function(sex) {
# if (sex == "male"){
#   model_used = "model3_trajectory_assignments"
#  df = df_male
# }
# else if (sex =="female"){
#  model_used = "model4_trajectory_assignments"
#  df = df_female
# }
# for (x in 16:ncol(df)) {
#  name <- colnames(df[,x])
#  assign("objectname", (paste(name,"~","SEP +", model_used, "+ Remoteness + Indigenous + adv + dis + resources + education_occupation + mother_home + father_home + mental_health + mother_race + father_race")))
#  expData <- assign(paste0("expData_", name), neImpute(eval(parse(text=objectname)), data = df))
#  assign("model", (paste(name,"~",("SEP0 + SEP1 + Remoteness + Indigenous + adv + dis + resources + education_occupation + mother_home + father_home + mental_health + mother_race + father_race"))))
#  outcome_results <- assign(paste0("ne_", name), neModel(model, expData = expData, se = "robust"))
#  assign(paste0(sex, "_effdecomp_", name), envir = globalenv(), neEffdecomp(outcome_results))
# }
# }
