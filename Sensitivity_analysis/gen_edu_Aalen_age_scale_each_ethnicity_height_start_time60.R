# input
# gen_edu_analysis_2022_04_18.R
# R4.1.3

# objective of this file
# Interaction of education*nativity for Chinese/Japanese/Filipino
# Adjustment: nativity, sex, ehr_ht_median (04/18/2022)
# Use imputed datasets (m=20) and combine the results apllying Rubin's rule ()
# Make a file for a figure for the main effect education (within ethnicity)
# Adjust for ehr_ht_median (04/18/2022)

# models
# Figueiras et al., Statistics in Medicine, 1998
# Aalen additive model
# Rod et al., Epidemiology 2012
# set , start.time = 60 (03/21/2022) 

#03/05/2022
#separate "Nativity" column in aalen_output function

#path to working directories
path_to_data <- "~/Library/CloudStorage/Box-Box/Asian_Americans_dementia_data/generation x education/Paper/2022-04-11-/Imputed_data/"
path_to_code <- "~/Library/CloudStorage/Box-Box/Asian_Americans_dementia_data/generation x education/Paper/2022-04-11-/Rcode/"
path_to_output <- "~/Library/CloudStorage/Box-Box/Asian_Americans_dementia_data/generation x education/Paper/2022-04-11-/Result/Sub_height/"

#load packages
library(data.table)
library(haven)
library(survival)
library(mice)
library(mitools)
library(tidyverse)
library(stringr)
library(openxlsx)
library(rlang)
library(timereg)

options(scipen = 999, digits=8)

#source scripts
source(paste0(path_to_code, "gen_edu_function_2022_02_27.R")) #interactions for Aalen models

#read gen_edu_analysis_2022_04_11.R -> data_post_imp
load(paste0(path_to_data,"gen_edu_analysis_2022_04_18.R"))
data_post_imp <- as.data.table(gen_edu_analysis)
nrow(data_post_imp) #294980 = 14749*20
nrow(data_post_imp)/20
colnames(data_post_imp)

str(data_post_imp)

#sanity check
#confirm that those with main_dem_v1_sample==0 are excluded
nrow(data_post_imp[main_dem_v1_sample==0,]) # 0
#confirm that those with main_dem_v1_fu_time==0 are excluded
nrow(data_post_imp[main_dem_v1_fu_time==0,]) # 0
#ethnicity
table(data_post_imp$ethnicity_rev, exclude = NULL)/20 # 2/3/5 = 6415/3314/5020 (Chinese/Japanese/Filipino)
table(data_post_imp$ethnicity_rev,data_post_imp$asian,exclude = NULL)
#flag indicating dementia diagnosis reason for end of follow-up
table(data_post_imp$main_dem_v1_end_type,data_post_imp$dem_end_flag,exclude = NULL)
#dichotomous education variable (<college, college+)
table(data_post_imp$education_rev,data_post_imp$college_edu,exclude = NULL)
#categorical education variable (<12, 12-college, college or more)
table(data_post_imp$education_rev,data_post_imp$education_cat,exclude = NULL)

#center `ehr_height_median` 
summary(data_post_imp$ehr_ht_median)
data_post_imp$height_65 <- data_post_imp$ehr_ht_median - 65
summary(data_post_imp$height_65)

#confirm that there is no NA (i.e., after multiple imputation)
data_post_imp_comp <- data_post_imp[complete.cases(data_post_imp)]
setequal(data_post_imp,data_post_imp_comp) #TRUE

#prepare datasets
data_chinese <- data_post_imp[ethnicity_rev==2,]
# data_chinese <- data_post_imp[chinese==1,]
data_japanese <- data_post_imp[ethnicity_rev==3,]
# data_japanese <- data_post_imp[japanese==1,]
data_filipino <- data_post_imp[ethnicity_rev==5,]
# data_filipino <- data_post_imp[filipino==1,]

#Aalen models
aalen_model_list_asian <- list()
aalen_model_list_chinese <- list()
aalen_model_list_japanese <- list()
aalen_model_list_filipino <- list()

# for(i in 1:2){
for(i in 1:max(data_post_imp$imp)){
  # subset_asian <- data_post_imp %>% filter(imp == 1)
  subset_asian <- data_post_imp %>% filter(imp == i)
  subset_chinese <- data_chinese %>% filter(imp == i)
  subset_japanese <- data_japanese %>% filter(imp == i)
  subset_filipino <- data_filipino %>% filter(imp == i)
  
  set.seed(i)
  # test <- 
  # aalen_model_list_asian[[1]] <-
  aalen_model_list_asian[[i]] <-
    aalen(Surv(survey_age, main_dem_v1_end_age, dem_end_flag) ~ 
            const(factor(college_edu))*const(factor(usaborn_rev)) + const(factor(female))
          + const(height_65),
          data=subset_asian, robust = TRUE, start.time = 60)
  
  aalen_model_list_chinese[[i]] <-
    aalen(Surv(survey_age, main_dem_v1_end_age, dem_end_flag) ~
            const(factor(college_edu))*const(factor(usaborn_rev)) + const(factor(female))
          + const(height_65),
          data=subset_chinese, robust = TRUE, start.time = 60)

  aalen_model_list_japanese[[i]] <-
    aalen(Surv(survey_age, main_dem_v1_end_age, dem_end_flag) ~
            const(factor(college_edu))*const(factor(usaborn_rev)) + const(factor(female))
          + const(height_65),
          data=subset_japanese, robust = TRUE, start.time = 60)

  aalen_model_list_filipino[[i]] <-
    aalen(Surv(survey_age, main_dem_v1_end_age, dem_end_flag) ~
            const(factor(college_edu))*const(factor(usaborn_rev)) + const(factor(female))
          + const(height_65),
          data=subset_filipino, robust = TRUE, start.time = 60)
  
}

#function for p-value after Rubin's rule
#https://thestatsgeek.com/2020/11/05/p-values-after-multiple-imputation-using-mitools-in-r/
MIcombineP <- function(MIcombineRes,digits=3) {
  tStat <- MIcombineRes$coefficients/sqrt(diag(MIcombineRes$variance))
  round(2*pt(-abs(tStat),df=MIcombineRes$df),digits) %>% t() %>% t()
}

#apply Rubin's rule
beta_asian <- MIextract(aalen_model_list_asian,fun=coef) %>% lapply(.,function(x){x[,1]})
# lapply(beta_asian, function(x){mean(x[4])}) %>% unlist() %>% mean()
beta_chinese <- MIextract(aalen_model_list_chinese,fun=coef) %>% lapply(.,function(x){x[,1]})
beta_japanese <- MIextract(aalen_model_list_japanese,fun=coef) %>% lapply(.,function(x){x[,1]})
beta_filipino <- MIextract(aalen_model_list_filipino,fun=coef) %>% lapply(.,function(x){x[,1]})
# MIextract(aalen_model_list_asian,fun=vcov)

MIcombine(beta_asian, MIextract(aalen_model_list_asian,fun=vcov)) %>% vcov()

result_row_names <- MIcombine(beta_asian, MIextract(aalen_model_list_asian,fun=vcov)) %>%
  summary %>% rownames()

aalen_result_raw_asian <- MIcombine(beta_asian, MIextract(aalen_model_list_asian,fun=vcov)) %>%
  summary %>% mutate(P_value = MIcombineP(MIcombine(beta_asian, MIextract(aalen_model_list_asian,fun=vcov))),
                     Variable = result_row_names, group="Asian")

aalen_result_raw_chinese <- MIcombine(beta_chinese, MIextract(aalen_model_list_chinese,fun=vcov)) %>% 
  summary %>% mutate(P_value = MIcombineP(MIcombine(beta_chinese, MIextract(aalen_model_list_chinese,fun=vcov))),
                     Variable = result_row_names, group="Chinese")

aalen_result_raw_japanese <- MIcombine(beta_japanese, MIextract(aalen_model_list_japanese,fun=vcov)) %>% 
  summary %>% mutate(P_value = MIcombineP(MIcombine(beta_japanese, MIextract(aalen_model_list_japanese,fun=vcov))),
                     Variable = result_row_names, group="Japanese")

aalen_result_raw_filipino <- MIcombine(beta_filipino, MIextract(aalen_model_list_filipino,fun=vcov)) %>% 
  summary %>% mutate(P_value = MIcombineP(MIcombine(beta_filipino, MIextract(aalen_model_list_filipino,fun=vcov))),
                     Variable = result_row_names, group="Filipino")

aalen_result_raw <- rbind(aalen_result_raw_asian, aalen_result_raw_chinese,
                        aalen_result_raw_japanese, aalen_result_raw_filipino)
rownames(aalen_result_raw) <- NULL
# aalen_result_raw %>% select()

#function for output
aalen_output <- function(ethnicity, py_unit = 1000){
  # result <- aalen_result_raw[aalen_result_raw$group=="Asian",]
  result <- aalen_result_raw[aalen_result_raw$group==ethnicity,]

  # college
  college <- round(py_unit*result[result$Variable == "const(factor(college_edu))1","results"], 2)
  conf_int <- paste0(" (",round(py_unit*result[result$Variable == "const(factor(college_edu))1","(lower"], 2),", ",
                     round(py_unit*result[result$Variable == "const(factor(college_edu))1","upper)"], 2),")")
  result_college <- data.table(Variable = "College",
                               Nativity = "Foreign-born", Estimates = college, CI = conf_int)
  # college for us-born
  # result_subpopulation <- gen_edu_HR_calc_college_us_born(input = "Asian")
  result_subpopulation <- gen_edu_additive_calc_college_us_born(input = ethnicity)
  college_sub <- round(py_unit*result_subpopulation[1], 2)
  conf_int <- paste0(" (",round(py_unit*result_subpopulation[2], 2),", ",round(py_unit*result_subpopulation[3], 2),")")
  result_college_us_born <- data.table(Variable = "College",
                                       Nativity = "US-born", Estimates = college_sub, CI = conf_int)
  
  result <- rbind(result_college,result_college_us_born)
  result[,Subpopulation:= ethnicity]
  names(result)[names(result) == "Estimates"] <- paste0("Estimates per ",as.character(py_unit), " person-years")
  names(result)[names(result) == "CI"] <- "95% CI"
  return(result)
}

aalen_result <- list()
aalen_result[["Asian"]] <- aalen_output(ethnicity = "Asian")
aalen_result[["Chinese"]] <- aalen_output(ethnicity = "Chinese")
aalen_result[["Japanese"]] <- aalen_output(ethnicity = "Japanese")
aalen_result[["Filipino"]] <- aalen_output(ethnicity = "Filipino")
aalen_result <- rbindlist(aalen_result)


#save the result
aalen_result_list <- list(aalen_result_main = aalen_result,
                        aalen_result_raw = aalen_result_raw)

write.xlsx(aalen_result_list,
           file = paste0(path_to_output,"gen_edu_binary_aalen_age_scale_height_2022_04_20_start_time60_v2.xlsx"),
           overwrite = TRUE)