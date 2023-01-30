# input
# gen_edu_analysis_2022_04_18.R
# R4.1.3

# objective of this file
# Interaction of three-category-education*nativity for Chinese/Japanese/Filipino
# Adjustment: nativity, sex
# Use imputed datasets (m=20) and combine the results apllying Rubin's rule ()
# Make a file for a figure for the main effect education (within ethnicity)

# Conduct LR tests for the interaction terms (05/08/2022)

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
source(paste0(path_to_code, "gen_edu_function_edu_category_2022_03_08.R")) #function for interaction

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

#set high school as the reference for the education variable ("education_cat")
data_post_imp$education_cat <- relevel(factor(data_post_imp$education_cat,ordered = F), ref = "High school")

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
            factor(education_cat) + factor(usaborn_rev) + factor(education_cat)*factor(usaborn_rev) + factor(female),
          data=subset_asian, id=subjid, robust = TRUE)
  
  cox_model_list_chinese[[i]] <-
    coxph(Surv(survey_age, main_dem_v1_end_age, dem_end_flag) ~ 
            factor(education_cat) + factor(usaborn_rev) + factor(education_cat)*factor(usaborn_rev) + factor(female),
          data=subset_chinese, id=subjid, robust = TRUE)
  
  cox_model_list_japanese[[i]] <-
    coxph(Surv(survey_age, main_dem_v1_end_age, dem_end_flag) ~ 
            factor(education_cat) + factor(usaborn_rev) + factor(education_cat)*factor(usaborn_rev) + factor(female),
          data=subset_japanese, id=subjid, robust = TRUE)
  
  cox_model_list_filipino[[i]] <-
    coxph(Surv(survey_age, main_dem_v1_end_age, dem_end_flag) ~ 
            factor(education_cat) + factor(usaborn_rev) + factor(education_cat)*factor(usaborn_rev) + factor(female),
          data=subset_filipino, id=subjid, robust = TRUE)
  
}

# #reduced model (without the interaction terms) (05/08/2022)
# cox_model_list_asian_reduced <- list()
# cox_model_list_chinese_reduced <- list()
# cox_model_list_japanese_reduced <- list()
# cox_model_list_filipino_reduced <- list()
# 
# for(i in 1:max(data_post_imp$imp)){
#   subset_asian_reduced <- data_post_imp %>% filter(imp == i)
#   subset_chinese_reduced <- data_chinese %>% filter(imp == i)
#   subset_japanese_reduced <- data_japanese %>% filter(imp == i)
#   subset_filipino_reduced <- data_filipino %>% filter(imp == i)
#   
#   cox_model_list_asian_reduced[[i]] <-
#     coxph(Surv(survey_age, main_dem_v1_end_age, dem_end_flag) ~ 
#             factor(education_cat) + factor(usaborn_rev) + factor(female),
#           data=subset_asian_reduced, id=subjid, robust = TRUE)
#   
#   cox_model_list_chinese_reduced[[i]] <-
#     coxph(Surv(survey_age, main_dem_v1_end_age, dem_end_flag) ~ 
#             factor(education_cat) + factor(usaborn_rev) + factor(female),
#           data=subset_chinese_reduced, id=subjid, robust = TRUE)
#   
#   cox_model_list_japanese_reduced[[i]] <-
#     coxph(Surv(survey_age, main_dem_v1_end_age, dem_end_flag) ~ 
#             factor(education_cat) + factor(usaborn_rev) + factor(female),
#           data=subset_japanese_reduced, id=subjid, robust = TRUE)
#   
#   cox_model_list_filipino_reduced[[i]] <-
#     coxph(Surv(survey_age, main_dem_v1_end_age, dem_end_flag) ~ 
#             factor(education_cat) + factor(usaborn_rev) + factor(female),
#           data=subset_filipino_reduced, id=subjid, robust = TRUE)
#   
# }

# # LR tests (05/08/2022)
# reduced_model_asian <- as.mira(cox_model_list_asian_reduced)
# bigger_model_asian <- as.mira(cox_model_list_asian)
# reduced_model_chinese <- as.mira(cox_model_list_chinese_reduced)
# bigger_model_chinese <- as.mira(cox_model_list_chinese)
# reduced_model_japanese <- as.mira(cox_model_list_japanese_reduced)
# bigger_model_japanese <- as.mira(cox_model_list_japanese)
# reduced_model_filipino <- as.mira(cox_model_list_filipino_reduced)
# bigger_model_filipino <- as.mira(cox_model_list_filipino)
# 
# LRT_asian <- D1(bigger_model_asian, reduced_model_asian)
# LRT_chinese <- D1(bigger_model_chinese, reduced_model_chinese)
# LRT_japanese <- D1(bigger_model_japanese, reduced_model_japanese)
# LRT_filipino <- D1(bigger_model_filipino, reduced_model_filipino)

# #save the result
# LRT_result_list <- list(LRT_asian = LRT_asian$result,
#                         LRT_chinese = LRT_chinese$result,
#                         LRT_japanese = LRT_japanese$result,
#                         LRT_filipino = LRT_filipino$result)
# 
# write.xlsx(LRT_result_list,
#            file = paste0(path_to_output,"LRT_three_cat_cox_age_scale_2022_05_08.xlsx"),
#            overwrite = TRUE)

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
  
  #foreign_born population
  # college
  college <- round(result[result$Variable == "factor(education_cat)College or more","HR"], 2)
  conf_int <- paste0(" (",round(result[result$Variable == "factor(education_cat)College or more","HR_lower_lim"], 2),", ",
                     round(result[result$Variable == "factor(education_cat)College or more","HR_upper_lim"], 2),")")
  result_college <- data.table(Variable = "College", Nativity = "Foreign-born", HR = college, CI = conf_int)
  # High_school (Reference)
  result_high_school <- data.table(Variable = "High school (ref.)", Nativity = "Foreign-born", HR = 1, CI = NA)
  # Less than high school
  less_than_high_school <- round(result[result$Variable == "factor(education_cat)Less than high school","HR"], 2)
  conf_int <- paste0(" (",round(result[result$Variable == "factor(education_cat)Less than high school","HR_lower_lim"], 2),", ",
                     round(result[result$Variable == "factor(education_cat)Less than high school","HR_upper_lim"], 2),")")
  result_less_than_high_school <- data.table(Variable = "Less than high school", Nativity = "Foreign-born",
                                             HR = less_than_high_school, CI = conf_int)
  
  #us_born population
  # college for "us_born" subpopulation
  result_subpopulation <- custom_HR_calc_education_cat_us_born(input = ethnicity, ctg = "College or more")
  college_sub <- round(result_subpopulation[1], 2)
  conf_int <- paste0(" (",round(result_subpopulation[2], 2),", ",round(result_subpopulation[3], 2),")")
  result_college_subpopulation <- data.table(Variable = "College", Nativity = "US-born",
                                             HR = college_sub, CI = conf_int)
  # High_school (Reference)
  result_high_school_subpopulation <- data.table(Variable = "High school (ref.)", 
                                                 Nativity = "US-born", HR = 1, CI = NA)
  # Less than high school for "us_born" subpopulation
  result_subpopulation <- custom_HR_calc_education_cat_us_born(input = ethnicity, ctg = "Less than high school")
  less_than_high_school_sub <- round(result_subpopulation[1], 2)
  conf_int <- paste0(" (",round(result_subpopulation[2], 2),", ",round(result_subpopulation[3], 2),")")
  result_less_than_high_school_subpopulation <- data.table(Variable = "Less than high school",
                                                           Nativity = "US-born", HR = less_than_high_school_sub, CI = conf_int)
  
  result <- rbind(result_college, result_high_school, result_less_than_high_school,
                  result_college_subpopulation, result_high_school_subpopulation,
                  result_less_than_high_school_subpopulation)
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
           file = paste0(path_to_output,"gen_edu_three_cat_cox_age_scale_2022_04_18_v3.xlsx"),
           overwrite = TRUE)


