# input
# gen_edu_analysis_2022_04_18.R
# R4.1.3

# objective of this file
# Interaction of three-category-education*nativity for Chinese/Japanese/Filipino
# Adjustment: nativity, sex
# Use imputed datasets (m=20) and combine the results apllying Rubin's rule ()
# Make a file for a figure for the main effect education (within ethnicity)

# models
# Figueiras et al., Statistics in Medicine, 1998
# Aalen additive model
# Rod et al., Epidemiology 2012
# set , start.time = 60 (03/21/2022) 

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
library(timereg)

options(scipen = 999, digits=8)

#source scripts
source(paste0(path_to_code, "gen_edu_function_edu_category_2022_03_08.R")) #function for interaction

#read gen_edu_analysis_2022_04_18.R -> data_post_imp
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
            const(factor(education_cat))*const(factor(usaborn_rev)) + const(factor(female)),
          data=subset_asian, robust = TRUE, start.time = 60)
  
  aalen_model_list_chinese[[i]] <-
    aalen(Surv(survey_age, main_dem_v1_end_age, dem_end_flag) ~
            const(factor(education_cat))*const(factor(usaborn_rev)) + const(factor(female)),
          data=subset_chinese, robust = TRUE, start.time = 60)

  aalen_model_list_japanese[[i]] <-
    aalen(Surv(survey_age, main_dem_v1_end_age, dem_end_flag) ~
            const(factor(education_cat))*const(factor(usaborn_rev)) + const(factor(female)),
          data=subset_japanese, robust = TRUE, start.time = 60)

  aalen_model_list_filipino[[i]] <-
    aalen(Surv(survey_age, main_dem_v1_end_age, dem_end_flag) ~
            const(factor(education_cat))*const(factor(usaborn_rev)) + const(factor(female)),
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

  #foreign_born population
  # college
  college <- round(py_unit*result[result$Variable == "const(factor(education_cat))College or more","results"], 2)
  conf_int <- paste0(" (",round(py_unit*result[result$Variable == "const(factor(education_cat))College or more","(lower"], 2),", ",
                     round(py_unit*result[result$Variable == "const(factor(education_cat))College or more","upper)"], 2),")")
  result_college <- data.table(Variable = "College",
                               Nativity = "Foreign-born", Estimates = college, CI = conf_int)
  # High_school (Reference)
  result_high_school <- data.table(Variable = "High school (ref.)", 
                                   Nativity = "Foreign-born", Estimates = 0, CI = NA)
  # Less than high school
  less_than_high_school <- round(py_unit*result[result$Variable == "const(factor(education_cat))Less than high school","results"], 2)
  conf_int <- paste0(" (",round(py_unit*result[result$Variable == "const(factor(education_cat))Less than high school","(lower"], 2),", ",
                                   round(py_unit*result[result$Variable == "const(factor(education_cat))Less than high school","upper)"], 2),")")
  result_less_than_high_school <- data.table(Variable = "Less than high school", Nativity = "Foreign-born",
                                             Estimates = less_than_high_school, CI = conf_int)
  
  #us_born population
  # college for "us_born" subpopulation
  # result_subpopulation <- gen_edu_additive_calc_education_cat_us_born(input = "Asian", ctg = "College or more")
  result_subpopulation <- gen_edu_additive_calc_education_cat_us_born(input = ethnicity, ctg = "College or more")
  college_sub <- round(py_unit*result_subpopulation[1], 2)
  conf_int <- paste0(" (",round(py_unit*result_subpopulation[2], 2),", ",round(py_unit*result_subpopulation[3], 2),")")
  result_college_subpopulation <- data.table(Variable = "College", Nativity = "US-born",
                                             Estimates = college_sub, CI = conf_int)
  # High_school (Reference)
  result_high_school_subpopulation <- data.table(Variable = "High school (ref.)", Nativity = "US-born", Estimates = 0, CI = NA)
  # Less than high school for "us_born" subpopulation
  result_subpopulation <- gen_edu_additive_calc_education_cat_us_born(input = ethnicity, ctg = "Less than high school")
  less_than_high_school_sub <- round(py_unit*result_subpopulation[1], 2)
  conf_int <- paste0(" (",round(py_unit*result_subpopulation[2], 2),", ",round(py_unit*result_subpopulation[3], 2),")")
  result_less_than_high_school_subpopulation <- data.table(Variable = "Less than high school",
                                                           Nativity = "US-born", Estimates = less_than_high_school_sub, CI = conf_int)
  
  result <- rbind(result_college, result_high_school, result_less_than_high_school,
                  result_college_subpopulation, result_high_school_subpopulation,
                  result_less_than_high_school_subpopulation)
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
           file = paste0(path_to_output,"gen_edu_three_cat_aalen_age_scale_2022_04_18_start_time60_v4.xlsx"),
           overwrite = TRUE)