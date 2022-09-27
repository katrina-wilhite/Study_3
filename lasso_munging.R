source("./load_data_by_sex.R")
library(regmed)
library(dplyr)
library(haven)

male_data()
female_data()

munge_lasso <- function(sex) {
  if (sex == "male"){
    d = df_male_domsp_weekday
  }
  else if (sex =="female"){
    d = df_female_domsp_weekday
  }
  #Prepare dataset
  relevant_wave6_data <- lsac_wave6 %>% 
    select(hicid, fcnfsad2:fcnfseo2d)
  relevant_wave8_data <- lsac_wave8 %>% 
    select(hicid, hapsoc:hasdqtb)
  domsp_wave6_lasso <- inner_join(d, relevant_wave6_data, by = "hicid")
  df_lasso <- inner_join(domsp_wave6_lasso, relevant_wave8_data, by = "hicid") 
  df_lasso <- na.omit(df_lasso)
  #Prepare mediator variables
  mediator_variables <- df_lasso %>% 
    select(active_transport_at_10:nighttime_sleep_at_10)
  mediator_list=as.list(mediator_variables)
  mediator_matrix <- matrix(unlist(mediator_list), ncol = 13, byrow = FALSE) 
  colnames(mediator_matrix) <- c("v1_active_transport", "v2_naps", "v3_education", "v4_leisure_SB", "v5_passive_transport", "v6_screen_time", "v7_self_care", "v8_social", "v9_sports", "v10_unst_LPA", "v11_unst_MVPA", "v12_household", "v13_sleep")
  df_mediators <- as.data.frame(mediator_matrix)
  df_mediators[df_mediators == 0] <- 0.1
  df_mediators <- scale(df_mediators) 
  ##Make interaction terms from mediator variables           
  for (x in 2:12){
    assign(paste0("interactions_", x), (t(apply(df_mediators, 1, combn, x, prod))))
    interaction_number <- get(paste0("interactions_",x))
    colnames(interaction_number) <- paste(combn(1:13, x, paste, collapse="V"), sep = "_")
    assign(paste0("interactions_", x), interaction_number)
  }
  interactions_13 <- df_mediators[,1]*df_mediators[,2]*df_mediators[,3]*df_mediators[,4]*df_mediators[,5]*df_mediators[,6]*df_mediators[,7]*df_mediators[,8]*df_mediators[,9]*df_mediators[,10]*df_mediators[,11]*df_mediators[,12]*df_mediators[,13]
  interactions_13 <- as.matrix(interactions_13)
  colnames(interactions_13) <- "1V2V3V4V5V6V7V8V9V10V11V12V13"
  assign(paste0(sex, "_lasso_mediators"), envir = globalenv(), cbind(interactions_2, interactions_3, interactions_4, interactions_5, interactions_6, interactions_7, interactions_8, interactions_9, interactions_10, interactions_11, interactions_12, interactions_13))
  #Prepare exposure variable 
 assign(paste0(sex, "_exposure"), envir = globalenv(), pull(df_lasso, SEP))
  #Select outcome variables 
  for (x in 123:ncol(df_lasso)) {
      name <- colnames(df_lasso[,x]) 
      assign(paste(name), df_lasso %>% 
      select(x) %>%
      scale())
      assign(paste0(name),get(name)[,1])
    }
  df_outcomes <- cbind(hapsoc, hahypr, haemot, hapeer,  hacondb, hasdqtb) 
  assign(paste0("df_", sex, "_outcomes"), envir = globalenv(), as.data.frame(df_outcomes))
  assign(paste0("df_", sex, "_lasso"), envir = globalenv(),df_lasso)
}

