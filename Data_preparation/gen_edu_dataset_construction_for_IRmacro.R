# input
# gen_edu_analysis_2022_04_18.R

# objective of this file
# construct dataset for IRmacro for the generation x education project

# models
# NA

#path to working directories
path_to_raw_data <- "~/Library/CloudStorage/Box-Box/Asian_Americans_dementia_data/analysis_data_tables/"
# path_to_nonimp_data <- "~/Library/CloudStorage/Box-Box/Asian_Americans_dementia_data/generation x education/Paper/"
path_to_imp_data <- "~/Library/CloudStorage/Box-Box/Asian_Americans_dementia_data/generation x education/Paper/2022-04-11-/Imputed_data/"
path_to_output <- "~/Library/CloudStorage/Box-Box/Asian_Americans_dementia_data/generation x education/Paper/Data_for_IR/"

#load packages
library(haven)
library(tidyverse)
library(data.table)
library(skimr)
library(rlang)
library(openxlsx)

# options(scipen = 999, digits=6)

#---- Importing Datsets ----
#Importing dataset using "haven" package
load(paste0(path_to_imp_data, "gen_edu_analysis_2022_04_18.R"))
data_post_im_macro <- gen_edu_analysis[gen_edu_analysis$imp==1,] #14749

# read aa_adrd_time_to_event.sas7bdat -> data_1
data_1 <- read_sas(paste0(path_to_raw_data,"aa_adrd_time_to_event.sas7bdat"))
data_1 <- as.data.table(data_1)
colnames(data_1) <- tolower(colnames(data_1))
dim(data_1) # 184929*182

# variable pick-up
data_2 <- data_1[,c("subjid", "mortality_ir_end_age", "mortality_ir_end_death_age")]
dim(data_2) # 184929*3

# merge "mortality_ir_end_age", "mortality_ir_end_death_age"
data_for_macro <- merge(data_post_im_macro, data_2, by = "subjid", all.x = TRUE)
summary(data_for_macro$mortality_ir_end_age)
summary(data_for_macro$mortality_ir_end_death_age)

#outputting R dataset to use the SAS mortality rate macro on it
write.csv(data_for_macro,
          paste0(path_to_output,
                 "gen_edu_data_for_IRmacro_2022_05_08.csv"))
