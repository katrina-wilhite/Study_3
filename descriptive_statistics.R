---
  output:
  html_document: default
word_document: default
pdf_document: default
---
  
```{r include=FALSE}
#Make Descriptive Statistics Table
install.packages("table1")
library("table1")
library("haven")
install.packages("tidyLPA")
library("tidyLPA")
library("dplyr")
library ("tidyr")
library("lubridate")
library("reshape2")
install.packages("janitor")
library("janitor")
library("crimCV")
library("kableExtra")
install.packages("magrittr")
library("stringr")
library("magrittr")
#install.packages("devto

```

```{r echo=FALSE}
#Select relevant columns for the descriptive statistics table
load(file = "Z:/LSAC dataset/Study_3/males_Study3_lasso.Rdata")
load(file = "Z:/LSAC dataset/Study_3/females_Study3_lasso.Rdata")

#Select relevant columns for descriptive statistics from male and female dataframes, then combine into one dataframe
df_males <- df_males %>% 
  dplyr::select(hicid, active_transport_at_10:nighttime_sleep_at_10, Age:Remoteness, fcnfsad2, fcnfsda2, fcnfser2, fcnfseo2, hapsoc:hasdqtb)
df_females <- df_females %>% 
  dplyr::select(hicid, active_transport_at_10:nighttime_sleep_at_10, Age:Remoteness, fcnfsad2, fcnfsda2, fcnfser2, fcnfseo2, hapsoc:hasdqtb)
df <- rbind(df_males, df_females, by = "hicid")

#Arrange the columns in the appropriate order
descriptive_statistics <- df %>% 
  relocate(c(Age, Indigenous, Remoteness, Sex, SEP), .before = active_transport_at_10) %>% 
  relocate(fcnfsad2:fcnfseo2, .before = active_transport_at_10) %>% 
  relocate(hapsoc:hasdqtb, .before = active_transport_at_10)   


descriptive_statistics <- data.frame(append(descriptive_statistics, list("Subject_Details" = "", "Strenghts_and_Difficulties_Questionnaire_Scores" = "", "Domain_Specific_Movement_Behaviours" = ""), after = 1))
descriptive_statistics %>% 
  relocate("Subject_Details", .before = Age) %>% 
  relocate("Domain_Specific_Movement_Behaviours", .before = active_transport_at_10) %>% 
  relocate("Strenghts_and_Difficulties_Questionnaire_Scores", .before = hapsoc) -> descriptive_statistics
#Clean column names
colnames(descriptive_statistics) <- gsub("_", " ", colnames(descriptive_statistics))
colnames(descriptive_statistics) <- str_to_title(colnames(descriptive_statistics))
colnames(descriptive_statistics) <- gsub("At", "at", colnames(descriptive_statistics))
colnames(descriptive_statistics) <- gsub("To", "to", colnames(descriptive_statistics))
colnames(descriptive_statistics) <- gsub("Light I", "Light-I", colnames(descriptive_statistics))
colnames(descriptive_statistics) <- gsub("Moderate", "Moderate-", colnames(descriptive_statistics))
colnames(descriptive_statistics) <- gsub("Vigorous", "Vigorous-", colnames(descriptive_statistics))
dplyr::rename(descriptive_statistics, "Socioeconomic Position" = Sep) -> descriptive_statistics
dplyr::rename(df, fcnfsad2 = "Advantage Disadvantage", fcnfsda2 = "Disadvantage", fcnfser2 = "Resources", fcnfseo2 ="Education and Occupation", hapscop = "Prosociality", hahpyr = "Hyperactivity", haemot = "Emotional Health", hapeer = "Peer Problems", hacondb = "Conduct Problems", hasdqtb = "Total Score")

#Define factor variables
descriptive_statistics$Indigenous <-
  factor(descriptive_statistics$Indigenous, levels = c(1, 2, 3, 4),
         labels = c("Not Aboriginal", "Aboriginal", "Torres Strait Islander", "Both"))
descriptive_statistics$Sex <-
  factor(descriptive_statistics$Sex, levels = c(1, 2),
         labels = c("Male", "Female"))
descriptive_statistics$Remoteness <-
  factor(descriptive_statistics$Remoteness, levels = c(0, 1, 2, 3, 4, 9),
         labels = c("Highly Accessible", "Accessible", "Moderately Accessible", "Remote", "Very Remote", "Not determined")) 


#Write rendering function for Mean and counts
my.render.cont <- function(x) {
  with(stats.apply.rounding(stats.default(x), digits=2), c("",
                                                           "Mean (SD)"=sprintf("%s (&plusmn; %s)", MEAN, SD)))
}
my.render.cat <- function(x) {
  c("", sapply(stats.default(x), function(y) with(y,
                                                  sprintf("%d (%0.0f %%)", FREQ, PCT))))
}
#trial 1a and 1b for stratifying by day of week 
table1(~ . | Sex, data=descriptive_statistics, footnote = "Values represent mean (SD) or count (%); Age is measured in years; Socioeconomic Position is a composite score of income,educational attainment, <br /> and occupation status; Movement Behaviours are measured in minutes", caption = "Table 2. Descriptive Statistics.", render.continuous=my.render.cont, render.categorical=my.render.cat)

```