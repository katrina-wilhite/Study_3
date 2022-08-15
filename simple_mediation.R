#Simple Mediation 

#Install appropriate libraries
#install.packages("medflex")
#Open libraries 
library(medflex)
library(dplyr)
library(haven)

#Load male dataset from Study 2, LSAC wave 6 data, and LSAC wave 8 data 
load(file = "C:/Users/katri/Documents/ACU/Study_2/Study_2/Study_2/df_males_domsp_weekday.RData") 

lsac_wave6 <- read_sas("Z:/LSAC dataset/General Release/Survey data/SAS/lsacgrb10.sas7bdat")

lsac_wave8 <- read_sas("Z:/LSAC dataset/General Release/Survey data/SAS/lsacgrb14.sas7bdat")

#Select relevant columns from male dataset (hicid, profile membership, age, remoteness, Indigenous status, SEP)
df_male_domsp_weekday %>% 
  select(hicid, model3_trajectory_assignments, Age, Remoteness, Indigenous, SEP) -> relevant_male_data

#Select relevant columns from LSAC wave 6 (SEIFA varialbes), parents living at home, parent mental health, parent ethnicity)  
lsac_wave6 %>% 
  select(hicid, fcnfsad2d, fcnfsda2d, fcnfser2d, fcnfseo2d, fmoth, ffath, fsc13a1i, zf09fm, zf09ff) -> wave_6_relevant_data
#Update column names to make sense 
colnames(wave_6_relevant_data)[2:10] <- c("advantage_disadvantage", "disadvantage", "resources", "education_occupation", "mother_home", "father_home", "mental_health", "mother_ethnicity", "father_ethnicity")

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

#Run analysis 
expData <- neImpute(prosocial ~ SEP + model3_trajectory_assignments + Remoteness + Indigenous + advantage_disadvantage + disadvantage + resources + education_occupation + mother_home + father_home + mental_health + mother_ethnicity + father_ethnicity, data = df_males_na_removed)
neProsocial <- neModel(prosocial ~ SEP0 + SEP1 + Remoteness + Indigenous + advantage_disadvantage + disadvantage + resources + education_occupation + mother_home + father_home + mental_health + mother_ethnicity + father_ethnicity, expData = expData, se = "robust")
summary(neProsocial)