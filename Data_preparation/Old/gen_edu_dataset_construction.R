# input
# aa_adrd_time_to_event.sas7bdat
# aa_adrd_cardiometabolic_tte.sas7bdat (04/18/2022)

# objective of this file
# construct dataset for multiple imputations and analysis for the generation x education project

# models
# NA

# path to working directories
path_to_data <- "~/Library/CloudStorage/Box-Box/Asian_Americans_dementia_data/analysis_data_tables/"
path_to_output <- "~/Library/CloudStorage/Box-Box/Asian_Americans_dementia_data/generation x education/Paper/2022-04-11-/"

# loading packages
library(data.table)
library(haven)
library(tidyverse)
library(skimr)

# read aa_adrd_time_to_event.sas7bdat -> data_1
data_1 <- read_sas(paste0(path_to_data,"aa_adrd_time_to_event.sas7bdat"))
data_1 <- as.data.table(data_1)
colnames(data_1) <- tolower(colnames(data_1))
colnames(data_1)
dim(data_1) # 184929*184

# read aa_adrd_time_to_event.sas7bdat -> data_height
data_height_pre <- read_sas(paste0(path_to_data,"aa_adrd_cardiometabolic_tte.sas7bdat"))
data_height_pre <- as.data.table(data_height_pre)
colnames(data_height_pre) <- tolower(colnames(data_height_pre))
colnames(data_height_pre)

data_height <- data_height_pre[,c("subjid", "ehr_ht_median")]
dim(data_height) # 184929*2

# # distribution check
# table(data_1$SURVEY_LANGUAGE) # 1601/183309/19 (Chinese/English/Spanish)
# table(data_1$EMPLOYMENT_RETIRED) # 54684/130245 (No or missing/Yes)
# table(data_1$STUDY) # 15188/169741 (CMHS/RPGEH)
# summary(data_1$SR_TOTAL_HEIGHT_M) # 1.067/1.600/1.676/1.753/2.362 meter (min/1st Q/median/3rd Q/max)

# variable pick-up
# add generalhealth, sr_total_height_in, sr_stroke, sr_diabetes, sr_hyp (04/11/2022)
data_2_pre <- data_1[,c("subjid", "main_dem_v1_sample", "main_dem_v1_end_type", "survey_age", 
                      "main_dem_v1_end_age", "main_dem_v1_fu_time",
                      "main_dem_v1_90flag", "death_90flag", "censor_90flag", 
                      "female", "sizeofhh", 
                      # "sr_total_height_in", (extract EHR-based height from data_height) (04/18/2022)
                      "survey_language", "income", "income_pp", "maritalstatus", "smoking_status",
                    "generalhealth", "sr_stroke", "sr_diabetes", "sr_hyp",
                      "ethnicity_rev", "asian", "education_rev", "usaborn_rev", "usabornfather_rev", "usabornmother_rev")]
dim(data_2_pre) # 184929*26

# merge "ehr_ht_median" (04/18/2022)
data_2 <- merge(data_2_pre, data_height, by = "subjid")
dim(data_2) # 184929*27
skim(data_2)

summary(data_2$ehr_ht_median) # (04/18/2022)

#subset dataset to `main_dem_v1_sample==1` and `main_dem_v1_fu_time!=0`
table(data_2$main_dem_v1_sample, exclude = NULL) #4535
xtabs(~main_dem_v1_sample+ethnicity_rev, exclude = NULL, data = data_2) # (05/18/2022)
xtabs(~main_dem_v1_sample+ethnicity_rev, exclude = NULL, data = data_2[ethnicity_rev==2|ethnicity_rev==3|ethnicity_rev==5,]) # (05/18/2022)
86+93+76 # 05/18/2022

data_2 <- copy(data_2[main_dem_v1_sample==1,])
xtabs(~main_dem_v1_sample+ethnicity_rev, exclude = NULL, data = data_2[ethnicity_rev==2|ethnicity_rev==3|ethnicity_rev==5,]) # (05/18/2022)
dim(data_2) # 180394*27

data_2 <- copy(data_2[main_dem_v1_fu_time!=0,])
dim(data_2) # 180394*27

#subset dataset to Chinese/Japanese/Filipino (ethnicity_rev==2/3/5)
data_asian <- data_2[ethnicity_rev==2|ethnicity_rev==3|ethnicity_rev==5,]
dim(data_asian) # 14749*27
xtabs(~ethnicity_rev, data = data_asian, exclude = NULL) #Chinese/Japanese/Filipino = 6415/3314/5020

#data cleaning
#flag indicating dementia diagnosis reason for end of follow-up
data_asian[,dem_end_flag:=ifelse((main_dem_v1_end_type=="DEMENTIA"),1,0)]
table(data_asian$main_dem_v1_end_type,data_asian$dem_end_flag,exclude = NULL)

#education for table1 output, but make sure to redefine after multiple imputations
table(data_asian$education_rev,exclude = NULL)
#dichotomous education variable (<college, college+)
data_asian[,college_edu:=ifelse((education_rev==5|education_rev==6),1,0)]
table(data_asian$education_rev,data_asian$college_edu,exclude = NULL)

#categorical education variable (<12, 12-college, college or more)
data_asian$education_cat <- "NA" #added on 02/16/2022
data_asian[(education_rev==1|education_rev==2),education_cat:="Less than high school"]
data_asian[(education_rev==3|education_rev==4),education_cat:="High school"]
data_asian[(education_rev==5|education_rev==6),education_cat:="College or more"]
table(data_asian$education_rev,data_asian$education_cat,exclude = NULL)
# data_asian$education_cat <- relevel(factor(data_asian$education_cat,ordered = F), ref = "High school")

#variable indicating white ethnicity
data_asian[,chinese:=ifelse((ethnicity_rev==2),1,0)]
data_asian[,japanese:=ifelse((ethnicity_rev==3),1,0)]
data_asian[,filipino:=ifelse((ethnicity_rev==5),1,0)]
# data_asian[,white:=ifelse((ethnicity_rev==9),1,0)]
# table(data_asian$ethnicity_rev,data_asian$white,exclude = NULL)
table(data_asian$ethnicity_rev,data_asian$asian,exclude = NULL)
xtabs(~ethnicity_rev + chinese, data = data_asian, exclude = NULL)
xtabs(~ethnicity_rev + japanese, data = data_asian, exclude = NULL)
xtabs(~ethnicity_rev + filipino, data = data_asian, exclude = NULL)

# Save the dataset
write_sas(data_asian, paste0(path_to_output,"gen_edu_data_for_mi_2022_04_18.sas7bdat"))

