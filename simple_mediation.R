#Simple Mediation 

#Install appropriate libraries
#install.packages("medflex")
#Open libraries 
library(medflex)
library(dplyr)
library(haven)

#Load male dataset from Study 2, LSAC wave 6 data, and LSAC wave 8 data 

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
colnames(wave_6_relevant_data)[2:10] <- c("adv", "dis", "resources", "education_occupation", "mother_home", "father_home", "mental_health", "mother_race", "father_race")

#Select relevant columns from LSAC Wave 8 (SDQ results) 
lsac_wave8 %>% 
  select(hicid, hapsoc:hasdqtb) -> wave_8_relevant_data
#Update column names to make sense
colnames(wave_8_relevant_data)[2:7] <- c("prosocial", "hyperactivity", "emotional", "peer", "conduct", "total")

#Combine columns to make new dataset 
inner_join(relevant_male_data, wave_6_relevant_data, by = "hicid") -> domsp_wave6

inner_join(domsp_wave6, wave_8_relevant_data, by = "hicid") -> df_males

colSums(df_males < -8)
#2 mental health, 9 mother race, 125 father race
df_males <- df_males[!(df_males$mental_health == -9),]
df_males <- df_males[!(df_males$mother_race == -9),]
df_males <- df_males[!(df_males$father_race == -9),]

#Check dataframe for missing data 
colSums(is.na(df_males))
#11 participants are missing SDQ data - this accounts for <1% of data and this information is essential for data anlaysis - therefore these participants will be removed from the dataset 
df_males_na_removed <- na.omit(df_males)


#Covert factor variables to factor 
df_males_na_removed$adv <- as.factor(df_males_na_removed$adv)
df_males_na_removed$dis <- as.factor(df_males_na_removed$dis)
df_males_na_removed$model3_trajectory_assignments <- as.factor(df_males_na_removed$model3_trajectory_assignments)
df_males_na_removed$Remoteness <- as.factor(df_males_na_removed$Remoteness)
df_males_na_removed$Indigenous <- as.factor(df_males_na_removed$Indigenous)
df_males_na_removed$resources <- as.factor(df_males_na_removed$resources)
df_males_na_removed$education_occupation <- as.factor(df_males_na_removed$education_occupation)
df_males_na_removed$mother_home <- as.factor(df_males_na_removed$mother_home)
df_males_na_removed$father_home <- as.factor(df_males_na_removed$father_home)
df_males_na_removed$mother_race <- as.factor(df_males_na_removed$mother_race)
df_males_na_removed$father_race <- as.factor(df_males_na_removed$father_race)
df_males_na_removed$mental_health <- as.factor(df_males_na_removed$mental_health)

#Run analysis 
df_males_na_removed$prosocial <- scale(df_males_na_removed$prosocial)

df_males_na_removed$mother_race <- df_males_na_removed$mother_race
df_males_na_removed$mother_race <- substr(df_males_na_removed$mother_race, 1, 1)
df_males_na_removed$father_race <- substr(df_males_na_removed$father_race, 1,1)
df_males_na_removed$mother_race <- as.factor(df_males_na_removed$mother_race)
df_males_na_removed$father_race <- as.factor(df_males_na_removed$father_race)
sapply(df_males_na_removed, class)

#expFit <- glm(model3_trajectory_assignments*SEP ~ Remoteness + Indigenous + adv + dis + resources + education_occupation + mother_home + father_home + mental_health + mother_race + father_race, data = df_males_na_removed)
expData <- neImpute(prosocial ~ SEP + model3_trajectory_assignments + Remoteness + Indigenous + adv + dis + resources + education_occupation + mother_home + father_home + mental_health + mother_race + father_race,  data = df_males_na_removed)
neProsocial <- neModel(prosocial ~ SEP0 + SEP1 + Remoteness + Indigenous + adv + dis + resources + education_occupation + mother_home + father_home + mental_health + mother_race + father_race, expData = expData, se = "robust")
summary(neProsocial)
str(neProsocial)

#I trired to "unlist" the results but this did not solve the error
#unlist(neProsocial) -> neProsocial2
head(neProsocial)

effdecomp <- neEffdecomp(neProsocial)
summary(effdecomp)

summary(expData)
head(expData)
expData2 <- neImpute(prosocial ~ SEP + model3_trajectory_assignments + Remoteness + Indigenous + adv + dis + resources + education_occupation + mother_home + father_home + mental_health + mother_ethnicity + father_ethnicity, family = poisson(link = "log"), data = df_males_na_removed)