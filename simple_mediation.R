#Simple Mediation 

#Install appropriate libraries
#install.packages("medflex")
#Open libraries 
library(medflex)
library(dplyr)
library(haven)

#Load male dataset from Study 2, LSAC wave 6 data, and LSAC wave 8 data 
<<<<<<< HEAD
#Update file path 
=======
>>>>>>> 05627d727055ab59f9ab00110e25336b72666e6d
load(file = "Z:/LSAC dataset/Study_2/Study_2/df_males_domsp_weekday.RData") 

lsac_wave6 <- read_sas("Z:/LSAC dataset/General Release/Survey data/SAS/lsacgrb10.sas7bdat")

lsac_wave8 <- read_sas("Z:/LSAC dataset/General Release/Survey data/SAS/lsacgrb14.sas7bdat")

#Select relevant columns from male dataset (hicid, profile membership, age, remoteness, Indigenous status, SEP)
df_male_domsp_weekday %>% 
  select(hicid, model3_trajectory_assignments, Age, Remoteness, Indigenous, SEP) -> relevant_male_data

#Select relevant columns from LSAC wave 6 (SEIFA varialbes), parents living at home, parent mental health, parent ethnicity)  
lsac_wave6 %>% 
  select(hicid, fcnfsad2d, fcnfsda2d, fcnfser2d, fcnfseo2d, fmoth, ffath, fsc13a1i, zf09fm, zf09ff) -> wave_6_relevant_data
#Update column names to make sense 
colnames(wave_6_relevant_data)[2:10] <- c("advantage_disadvantage", "disadvantage", "resources", "education_occupation", "mother_home", "father_home", "mental_health", "mother_race", "father_race")

#Select relevant columns from LSAC Wave 8 (SDQ results) 
lsac_wave8 %>% 
  select(hicid, hapsoc:hasdqtb) -> wave_8_relevant_data
#Update column names to make sense
colnames(wave_8_relevant_data)[2:7] <- c("prosocial", "hyperactivity", "emotional", "peer", "conduct", "total")

#Combine columns to make new dataset 
inner_join(relevant_male_data, wave_6_relevant_data, by = "hicid") -> domsp_wave6

inner_join(domsp_wave6, wave_8_relevant_data, by = "hicid") -> df_males
#Check dataframe for missing data 
colSums(is.na(df_males))
#11 participants are missing SDQ data - this accounts for <1% of data and this information is essential for data anlaysis - therefore these participants will be removed from the dataset 
df_males_na_removed <- na.omit(df_males)


df_males_na_removed$model3_trajectory_assignments <- as.factor(df_males_na_removed$model3_trajectory_assignments) 
#Run analysis 
df_males_na_removed[,"prosocial"] %>% 
  mutate_at(("prosocial"), ~(scale(.) %>% as.vector)) -> df_males_scaled

<<<<<<< HEAD
#I tried to "fit" a model but this did not solve the error "Error in varterms[sapply(cov, grep, varterms)] : invalid subscript type 'list'"
=======
#Try to fit model to population to see if this will fix error in "neEffdecomp" line
>>>>>>> 05627d727055ab59f9ab00110e25336b72666e6d
#expFit <- glm(model3_trajectory_assignments*SEP ~ Remoteness + Indigenous + advantage_disadvantage + disadvantage + resources + education_occupation + mother_home + father_home + mental_health + mother_race + father_race, data = df_males_na_removed)
expData <- neImpute(prosocial ~ SEP + model3_trajectory_assignments + Remoteness + Indigenous + advantage_disadvantage + disadvantage + resources + education_occupation + mother_home + father_home + mental_health + mother_race + father_race, data = df_males_na_removed)
neProsocial <- neModel(prosocial ~ SEP0 + SEP1 + Remoteness + Indigenous + advantage_disadvantage + disadvantage + resources + education_occupation + mother_home + father_home + mental_health + mother_race + father_race, expData = expData, se = "robust")
summary(neProsocial)
str(neProsocial)
<<<<<<< HEAD
#I trired to "unlist" the results but this did not solve the error
#unlist(neProsocial) -> neProsocial2
head(neProsocial)
=======

>>>>>>> 05627d727055ab59f9ab00110e25336b72666e6d
effdecomp <- neEffdecomp(neProsocial)
summary(effdecomp)

summary(expData)
head(expData)
expData2 <- neImpute(prosocial ~ SEP + model3_trajectory_assignments + Remoteness + Indigenous + advantage_disadvantage + disadvantage + resources + education_occupation + mother_home + father_home + mental_health + mother_ethnicity + father_ethnicity, family = poisson(link = "log"), data = df_males_na_removed)