library(dplyr)
#source("./load_data_by_sex.R")

munge <- function(sex) {
  if (sex == "male"){
    model_used = "model3_trajectory_assignments"
    df = df_male_domsp_weekday
    wave6 = as.array(c("hicid","fcnfsad2d","fcnfsda2d","fcnfser2d","fcnfseo2d","fmoth","ffath","fsc13a1i","zf09fm","zf09ff","fhs36e","fhs36c","fhs36a","fhs36f", "fhs36g"))
    rename6 = as.array(c("adv" = "fcnfsad2d", "dis" = "fcnfsda2d", "resources"= "fcnfser2d", "mother_home" = "fmoth", "father_home" = "ffath", "mental_health" = "fsc13a1i", "mother_race" = "zf09fm", "father_race" = "zf09ff", "growth" = "fhs36e", "hair" = "fhs36c", "skin" = "fhs36a", "voice" = "fhs36f", "facial" = "fhs36g"))
    }
  else if (sex =="female"){
    model_used = "model4_trajectory_assignments"
    df = df_female_domsp_weekday
    wave6 = as.array(c("hicid","fcnfsad2d","fcnfsda2d","fcnfser2d","fcnfseo2d","fmoth","ffath","fsc13a1i","zf09fm","zf09ff","fhs36e","fhs36c","fhs36a","fhs36d"))
    rename6 = as.array(c("adv" = "fcnfsad2d", "dis" = "fcnfsda2d", "resources"= "fcnfser2d", "mother_home" = "fmoth", "father_home" = "ffath", "mental_health" = "fsc13a1i", "mother_race" = "zf09fm", "father_race" = "zf09ff", "growth" = "fhs36e", "hair" = "fhs36c", "skin" = "fhs36a", "breast" = "fhs36d"))
  }
  relevant_sex_data <- df %>%
    dplyr::select(hicid, model_used, Age, Remoteness, Indigenous, SEP)
  # Select relevant columns from LSAC wave 6 (SEIFA varialbes), parents living at home, parent mental health, parent ethnicity)
  wave_6_relevant_data <- lsac_wave6 %>%
    dplyr::select(wave6) %>% 
    dplyr::rename(rename6)
  wave_8_relevant_data <- lsac_wave8 %>%
    dplyr::select(hicid, hapsoc:hasdqtb) %>% 
    dplyr::rename("prosocial" = "hapsoc", "hyperactivity" = "hahypr", "emotional" = "haemot", "peer" = "hapeer", "conduct" = "hacondb", "total" = "hasdqtb")
  domsp_wave6 <- inner_join(relevant_sex_data, wave_6_relevant_data, by = "hicid")
  df_sex <- inner_join(domsp_wave6, wave_8_relevant_data, by = "hicid")
  df_sex[!(df_sex$mental_health == -9), ]
  df_sex <- na.omit(df_sex)
  df_sex[(df_sex$mother_race == 1101),]
  df_sex <- df_sex %>% 
    mutate(mother_race=recode(mother_race, '1101' = '1', '-9' = '2')) 
  df_sex$mother_race[is.na(df_sex$mother_race)] <- 0
  df_sex$mother_race <- as.factor(df_sex$mother_race)
  df_sex <- df_sex %>% 
    mutate(father_race=recode(father_race, '1101' = '1', '-9' = '2')) 
  df_sex$father_race[is.na(df_sex$father_race)] <- 0
  df_sex$father_race <- as.factor(df_sex$father_race) 
  factor_columns <- c(model_used, "Remoteness", "Indigenous", "adv", "dis", "resources", "education_occupation", "mother_home", "father_home", "mental_health")
  for (x in factor_columns) {
    name <- colnames(df_sex[,x])
    df_sex[[name]] <- as.factor(df_sex[[name]])
  }
  cols <- c("prosocial", "hyperactivity", "emotional", "peer", "conduct", "total")
  df_sex <- df_sex %>% 
    mutate_at(c("prosocial", "hyperactivity", "emotional", "peer", "conduct", "total"), scale)
  assign(paste0("df_",sex), envir = globalenv(), df_sex)
}

