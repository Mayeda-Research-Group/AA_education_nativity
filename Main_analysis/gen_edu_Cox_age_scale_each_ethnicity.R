# input
# gen_edu_analysis_2022_04_18.R
# R4.1.3

# objective of this file
# Interaction of education*nativity for Chinese/Japanese/Filipino
# Adjustment: nativity, sex
# Use imputed datasets (m=20) and combine the results apllying Rubin's rule ()
# Make a file for a figure for the main effect education (within ethnicity)

#03/05/2022
#separate "Nativity" column in cox_output function

# models
# Cox proportional hazards model
# Estimate HR for each subpopulation
# Figueiras et al., Statistics in Medicine, 1998

#path to working directories
path_to_data <- "~/Library/CloudStorage/Box-Box/Asian_Americans_dementia_data/generation x education/Paper/2022-04-11-/Imputed_data/"
path_to_code <- "~/Library/CloudStorage/Box-Box/Asian_Americans_dementia_data/generation x education/Paper/2022-04-11-/Rcode/"
path_to_output <- "~/Library/CloudStorage/Box-Box/Asian_Americans_dementia_data/generation x education/Paper/2022-04-11-/Result/"

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

options(scipen = 999, digits=8)

#source scripts
source(paste0(path_to_code, "gen_edu_function_2022_02_27.R")) #interaction for Cox models

#read gen_edu_analysis_2022_04_11.R -> data_post_imp
load(paste0(path_to_data,"gen_edu_analysis_2022_04_18.R"))
data_post_imp <- as.data.table(gen_edu_analysis)
nrow(data_post_imp) #294980 = 14749*20
nrow(data_post_imp)/20
colnames(data_post_imp)

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

#Cox models
cox_model_list_asian <- list()
cox_model_list_chinese <- list()
cox_model_list_japanese <- list()
cox_model_list_filipino <- list()

for(i in 1:max(data_post_imp$imp)){
  subset_asian <- data_post_imp %>% filter(imp == i)
  subset_chinese <- data_chinese %>% filter(imp == i)
  subset_japanese <- data_japanese %>% filter(imp == i)
  subset_filipino <- data_filipino %>% filter(imp == i)
  
  cox_model_list_asian[[i]] <-
    coxph(Surv(survey_age, main_dem_v1_end_age, dem_end_flag) ~ 
            factor(college_edu) + factor(usaborn_rev) + factor(college_edu)*factor(usaborn_rev) + factor(female),
          data=subset_asian, id=subjid, robust = TRUE)
  
  cox_model_list_chinese[[i]] <-
    coxph(Surv(survey_age, main_dem_v1_end_age, dem_end_flag) ~ 
            factor(college_edu) + factor(usaborn_rev) + factor(college_edu)*factor(usaborn_rev) + factor(female),
          data=subset_chinese, id=subjid, robust = TRUE)
  
  cox_model_list_japanese[[i]] <-
    coxph(Surv(survey_age, main_dem_v1_end_age, dem_end_flag) ~ 
            factor(college_edu) + factor(usaborn_rev) + factor(college_edu)*factor(usaborn_rev) + factor(female),
          data=subset_japanese, id=subjid, robust = TRUE)
  
  cox_model_list_filipino[[i]] <-
    coxph(Surv(survey_age, main_dem_v1_end_age, dem_end_flag) ~ 
            factor(college_edu) + factor(usaborn_rev) + factor(college_edu)*factor(usaborn_rev) + factor(female),
          data=subset_filipino, id=subjid, robust = TRUE)
  
}

#function for p-value after Rubin's rule
#https://thestatsgeek.com/2020/11/05/p-values-after-multiple-imputation-using-mitools-in-r/
MIcombineP <- function(MIcombineRes,digits=3) {
  tStat <- MIcombineRes$coefficients/sqrt(diag(MIcombineRes$variance))
  round(2*pt(-abs(tStat),df=MIcombineRes$df),digits) %>% t() %>% t()
}

#apply Rubin's rule
result_row_names <- MIcombine(cox_model_list_asian) %>% summary %>% rownames()
cox_result_raw_asian <- MIcombine(cox_model_list_asian) %>% summary %>%
  mutate(P_value = MIcombineP(MIcombine(cox_model_list_asian)), Variable = result_row_names, HR = exp(results), 
         HR_lower_lim = exp(`(lower`), HR_upper_lim = exp(`upper)`), group="Asian")

cox_result_raw_chinese <- MIcombine(cox_model_list_chinese) %>% summary %>%
  mutate(P_value = MIcombineP(MIcombine(cox_model_list_chinese)), Variable = result_row_names, HR = exp(results),
         HR_lower_lim = exp(`(lower`), HR_upper_lim = exp(`upper)`), group="Chinese")

cox_result_raw_japanese <- MIcombine(cox_model_list_japanese) %>% summary %>%
  mutate(P_value = MIcombineP(MIcombine(cox_model_list_japanese)), Variable = result_row_names, HR = exp(results), 
         HR_lower_lim = exp(`(lower`), HR_upper_lim = exp(`upper)`), group="Japanese")

cox_result_raw_filipino <- MIcombine(cox_model_list_filipino) %>% summary %>%
  mutate(P_value = MIcombineP(MIcombine(cox_model_list_filipino)), Variable = result_row_names, HR = exp(results), 
         HR_lower_lim = exp(`(lower`), HR_upper_lim = exp(`upper)`), group="Filipino")

cox_result_raw <- rbind(cox_result_raw_asian, cox_result_raw_chinese,
                        cox_result_raw_japanese, cox_result_raw_filipino)
rownames(cox_result_raw) <- NULL
# cox_result_raw %>% select()

#function for output
cox_output <- function(ethnicity){
  # result <- cox_result_raw[cox_result_raw$group=="Asian",]
  result <- cox_result_raw[cox_result_raw$group==ethnicity,]

  # college
  college <- round(result[result$Variable == "factor(college_edu)1","HR"], 2)
  conf_int <- paste0(" (",round(result[result$Variable == "factor(college_edu)1","HR_lower_lim"], 2),", ",
                     round(result[result$Variable == "factor(college_edu)1","HR_upper_lim"], 2),")")
  result_college <- data.table(Variable = "College",
                               Nativity = "Foreign-born", HR = college, CI = conf_int)
  # college for us-born
  # result_subpopulation <- gen_edu_HR_calc_college_us_born(input = "Asian")
  result_subpopulation <- gen_edu_HR_calc_college_us_born(input = ethnicity)
  college_sub <- round(result_subpopulation[1], 2)
  conf_int <- paste0(" (",round(result_subpopulation[2], 2),", ",round(result_subpopulation[3], 2),")")
  result_college_us_born <- data.table(Variable = "College",
                                       Nativity = "US-born", HR = college_sub, CI = conf_int)
  
  result <- rbind(result_college,result_college_us_born)
  result[,Subpopulation:= ethnicity]
  names(result)[names(result) == "HR"] <- "Estimates (HR)"
  names(result)[names(result) == "CI"] <- "95% CI"
  return(result)
}

cox_result <- list()
cox_result[["Asian"]] <- cox_output(ethnicity = "Asian")
cox_result[["Chinese"]] <- cox_output(ethnicity = "Chinese")
cox_result[["Japanese"]] <- cox_output(ethnicity = "Japanese")
cox_result[["Filipino"]] <- cox_output(ethnicity = "Filipino")
cox_result <- rbindlist(cox_result)


#save the result
cox_result_list <- list(cox_result_main = cox_result,
                        cox_result_raw = cox_result_raw)

write.xlsx(cox_result_list,
           file = paste0(path_to_output,"gen_edu_binary_cox_age_scale_2022_04_18_v2.xlsx"),
           overwrite = TRUE)


