library(regmed)
library(dplyr)
library(haven)

munge_lasso <- function(sex) {
  if (sex == "male") {
    d <- df_male_domsp_weekday
  } else if (sex == "female") {
    d <- df_female_domsp_weekday
  }
  # Prepare dataset
  relevant_wave6_data <- lsac_wave6 %>%
    select(hicid, fcnfsad2:fcnfseo2d)
  relevant_wave8_data <- lsac_wave8 %>%
    select(hicid, hapsoc:hasdqtb)
  domsp_wave6_lasso <- inner_join(d, relevant_wave6_data, by = "hicid")
  df_lasso <- inner_join(domsp_wave6_lasso, relevant_wave8_data, by = "hicid")
  df_lasso <- na.omit(df_lasso)
  # Prepare mediator variables
  mediator_variables <- df_lasso %>%
    select(active_transport_at_10:nighttime_sleep_at_10)
  mediator_list <- as.list(mediator_variables)
  mediator_matrix <- matrix(unlist(mediator_list), ncol = 13, byrow = FALSE)
  colnames(mediator_matrix) <- c(
    "v1_active_transport", "v2_naps", "v3_education", "v4_leisure_SB",
    "v5_passive_transport", "v6_screen_time", "v7_self_care", "v8_social",
    "v9_sports", "v10_unst_LPA", "v11_unst_MVPA", "v12_household", "v13_sleep"
  )
  df_mediators <- as.data.frame(mediator_matrix)
  df_mediators[df_mediators == 0] <- 0.1
  df_mediators <- scale(df_mediators)
  ## Make interaction terms from mediator variables
  for (x in 2:12) {
    assign(
      paste0("interactions_", x), (t(apply(df_mediators, 1, combn, x, prod)))
    )
    interaction_number <- get(paste0("interactions_", x))
    colnames(interaction_number) <-
      paste(combn(1:13, x, paste, collapse = "V"), sep = "_")
    assign(paste0("interactions_", x), interaction_number)
  }
  interactions_13 <-
    df_mediators[, 1] * df_mediators[, 2] * df_mediators[, 3] *
      df_mediators[, 4] * df_mediators[, 5] * df_mediators[, 6] *
      df_mediators[, 7] * df_mediators[, 8] * df_mediators[, 9] *
      df_mediators[, 10] * df_mediators[, 11] * df_mediators[, 12] *
      df_mediators[, 13]
  interactions_13 <- as.matrix(interactions_13)
  colnames(interactions_13) <- "1V2V3V4V5V6V7V8V9V10V11V12V13"
  assign(paste0(sex, "_lasso_mediators"),
    envir = globalenv(),
    cbind(
      interactions_2, interactions_3, interactions_4, interactions_5,
      interactions_6, interactions_7, interactions_8, interactions_9,
      interactions_10, interactions_11, interactions_12, interactions_13
    )
  )
  # Prepare exposure variable
  assign(paste0(sex, "_exposure"), envir = globalenv(), pull(df_lasso, SEP))
  # Select outcome variables
  for (x in 123:ncol(df_lasso)) {
    name <- colnames(df_lasso[, x])
    assign(paste(name), df_lasso %>%
      select(all_of(x)) %>%
      scale())
    assign(paste0(name), get(name)[, 1])
  }
  df_outcomes <- cbind(hapsoc, hahypr, haemot, hapeer, hacondb, hasdqtb)
  assign(paste0("df_", sex, "_outcomes"),
    envir = globalenv(), as.data.frame(df_outcomes)
  )
  assign(paste0("df_", sex, "_lasso"), envir = globalenv(), df_lasso)
}


munge_lasso_two_way_interactions <- function(sex) {
  if (sex == "male") {
    d <- df_male_domsp_weekday
  } else if (sex == "female") {
    d <- df_female_domsp_weekday
  }
  # Prepare dataset
  relevant_wave6_data <- lsac_wave6 %>%
    select(hicid, fcnfsad2:fcnfseo2d)
  relevant_wave8_data <- lsac_wave8 %>%
    select(hicid, hapsoc:hasdqtb)
  domsp_wave6_lasso <- inner_join(d, relevant_wave6_data, by = "hicid")
  df_lasso <- inner_join(domsp_wave6_lasso, relevant_wave8_data, by = "hicid")
  df_lasso <- na.omit(df_lasso)
  # Prepare mediator variables
  mediator_variables <- df_lasso %>%
    select(active_transport_at_10:nighttime_sleep_at_10)
  mediator_list <- as.list(mediator_variables)
  mediator_matrix <- matrix(unlist(mediator_list), ncol = 13, byrow = FALSE)
  colnames(mediator_matrix) <- c("v1_active_transport", "v2_naps", "v3_education", "v4_leisure_SB", "v5_passive_transport", "v6_screen_time", "v7_self_care", "v8_social", "v9_sports", "v10_unst_LPA", "v11_unst_MVPA", "v12_household", "v13_sleep")
  df_mediators <- as.data.frame(mediator_matrix)
  df_mediators <- scale(df_mediators)
  ## Make interaction terms from mediator variables
  two_way_interaction <- t(apply(df_mediators, 1, combn, 2, prod))
  colnames(two_way_interaction) <- paste(combn(1:13, 2, paste, collapse = "V"), sep = "_")
  assign(paste0(sex, "_lasso_mediators_two"), envir = globalenv(), two_way_interaction)
  # Prepare exposure variable
  assign(paste0(sex, "_exposure"), envir = globalenv(), pull(df_lasso, SEP))
  # Select outcome variables
  for (x in 123:ncol(df_lasso)) {
    name <- colnames(df_lasso[, x])
    assign(paste(name), df_lasso %>%
      select(all_of(x)) %>%
      scale())
    assign(paste0(name), get(name)[, 1])
  }
  df_outcomes <- cbind(hapsoc, hahypr, haemot, hapeer, hacondb, hasdqtb)
  assign(paste0("df_", sex, "_outcomes"), envir = globalenv(), as.data.frame(df_outcomes))
  assign(paste0("df_", sex, "_lasso"), envir = globalenv(), df_lasso)
}


munge_lasso_general_interactions <- function(sex) {
  if (sex == "male") {
    d <- df_male_domsp_weekday
  } else if (sex == "female") {
    d <- df_female_domsp_weekday
  }
  # Prepare dataset
  relevant_wave6_data <- lsac_wave6 %>%
    select(hicid, fcnfsad2:fcnfseo2d)
  relevant_wave8_data <- lsac_wave8 %>%
    select(hicid, hapsoc:hasdqtb)
  domsp_wave6_lasso <- inner_join(d, relevant_wave6_data, by = "hicid")
  df_lasso <- inner_join(domsp_wave6_lasso, relevant_wave8_data, by = "hicid")
  df_lasso <- na.omit(df_lasso)
  # Prepare mediator variables
  mediator_variables <- df_lasso %>%
    select(LPA_at_10:sleep_at_10)
  mediator_list <- as.list(mediator_variables)
  mediator_matrix <- matrix(unlist(mediator_list), ncol = 4, byrow = FALSE)
  colnames(mediator_matrix) <- c("v1_light", "v2_moderate", "v3_sedentary", "v4_sleep")
  df_mediators <- as.data.frame(mediator_matrix)
  df_mediators <- scale(df_mediators)
  ## Make interaction terms from mediator variables
  two_way_interaction <- t(apply(df_mediators, 1, combn, 2, prod))
  colnames(two_way_interaction) <- paste(combn(1:4, 2, paste, collapse = "V"), sep = "_")
  assign(paste0(sex, "_lasso_mediators_general_interactions"), envir = globalenv(), two_way_interaction)
  # Prepare exposure variable
  assign(paste0(sex, "_exposure"), envir = globalenv(), pull(df_lasso, SEP))
  # Select outcome variables
  for (x in 123:ncol(df_lasso)) {
    name <- colnames(df_lasso[, x])
    assign(paste(name), df_lasso %>%
      select(all_of(x)) %>%
      scale())
    assign(paste0(name), get(name)[, 1])
  }
  df_outcomes <- cbind(hapsoc, hahypr, haemot, hapeer, hacondb, hasdqtb)
  assign(paste0("df_", sex, "_outcomes"), envir = globalenv(), as.data.frame(df_outcomes))
  assign(paste0("df_", sex, "_lasso"), envir = globalenv(), df_lasso)
}

munge_lasso_single <- function(sex) {
  if (sex == "male") {
    d <- df_male_domsp_weekday
  } else if (sex == "female") {
    d <- df_female_domsp_weekday
  }
  # Prepare dataset
  relevant_wave6_data <- lsac_wave6 %>%
    select(hicid, fcnfsad2:fcnfseo2d)
  relevant_wave8_data <- lsac_wave8 %>%
    select(hicid, hapsoc:hasdqtb)
  domsp_wave6_lasso <- inner_join(d, relevant_wave6_data, by = "hicid")
  df_lasso <- inner_join(domsp_wave6_lasso, relevant_wave8_data, by = "hicid")
  df_lasso <- na.omit(df_lasso)
  # Prepare mediator variables
  mediator_variables <- df_lasso %>%
    select(active_transport_at_10:nighttime_sleep_at_10)
  mediator_list <- as.list(mediator_variables)
  mediator_matrix <- matrix(unlist(mediator_list), ncol = 13, byrow = FALSE)
  colnames(mediator_matrix) <- c("v1_active_transport", "v2_naps", "v3_education", "v4_leisure_SB", "v5_passive_transport", "v6_screen_time", "v7_self_care", "v8_social", "v9_sports", "v10_unst_LPA", "v11_unst_MVPA", "v12_household", "v13_sleep")
  df_mediators <- as.data.frame(mediator_matrix)
  df_mediators[df_mediators == 0] <- 0.1
  df_mediators <- scale(df_mediators)
  assign(paste0(sex, "_lasso_mediators_single"), envir = globalenv(), df_mediators)

  # Prepare exposure variable
  assign(paste0(sex, "_exposure"), envir = globalenv(), pull(df_lasso, SEP))
  # female_exposure
  # Select outcome variables
  for (x in 123:ncol(df_lasso)) {
    name <- colnames(df_lasso[, x])
    assign(paste(name), df_lasso %>%
      select(all_of(x)) %>%
      scale())
    assign(paste0(name), get(name)[, 1])
  }
  df_outcomes <- cbind(hapsoc, hahypr, haemot, hapeer, hacondb, hasdqtb)
  assign(paste0("df_", sex, "_outcomes"), envir = globalenv(), as.data.frame(df_outcomes))
  assign(paste0("df_", sex, "_lasso"), envir = globalenv(), df_lasso)
}


# Lasso munge for general movement behaviours
munge_lasso_general <- function(sex) {
  if (sex == "male") {
    d <- df_male_domsp_weekday
  } else if (sex == "female") {
    d <- df_female_domsp_weekday
  }
  # Prepare dataset
  relevant_wave6_data <- lsac_wave6 %>%
    select(hicid, fcnfsad2:fcnfseo2d)
  relevant_wave8_data <- lsac_wave8 %>%
    select(hicid, hapsoc:hasdqtb)
  domsp_wave6_lasso <- inner_join(d, relevant_wave6_data, by = "hicid")
  df_lasso <- inner_join(domsp_wave6_lasso, relevant_wave8_data, by = "hicid")
  df_lasso <- na.omit(df_lasso)
  # Prepare mediator variables
  mediator_variables <- df_lasso %>%
    select(LPA_at_10:sleep_at_10)
  mediator_list <- as.list(mediator_variables)
  mediator_matrix <- matrix(unlist(mediator_list), ncol = 4, byrow = FALSE)
  colnames(mediator_matrix) <- c("v1_light", "v2_moderate", "v3_sedentary", "v4_sleep")
  df_mediators <- as.data.frame(mediator_matrix)
  df_mediators[df_mediators == 0] <- 0.1
  df_mediators <- scale(df_mediators)
  assign(paste0(sex, "_lasso_mediators_general"), envir = globalenv(), df_mediators)

  # Prepare exposure variable
  assign(paste0(sex, "_exposure"), envir = globalenv(), pull(df_lasso, SEP))
  # female_exposure
  # Select outcome variables
  for (x in 123:ncol(df_lasso)) {
    name <- colnames(df_lasso[, x])
    assign(paste(name), df_lasso %>%
      select(all_of(x)) %>%
      scale())
    assign(paste0(name), get(name)[, 1])
  }
  df_outcomes <- cbind(hapsoc, hahypr, haemot, hapeer, hacondb, hasdqtb)
  assign(paste0("df_", sex, "_outcomes"), envir = globalenv(), as.data.frame(df_outcomes))
  assign(paste0("df_", sex, "_lasso"), envir = globalenv(), df_lasso)
}
