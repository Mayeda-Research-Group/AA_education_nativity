# input
# gen_edu_data_for_mi_2022_04_18.sas7bdat
# gen_edu_analysis_2022_04_18.R
# R4.1.3

# objective of this file
# Two types of Table1
#1 one with rows for % missing
#2 one for stacked imputed data

#change a format for the paper manuscript (03/30/2022)
# add generalhealth, sr_stroke, sr_diabetes, sr_hyp, and ehr_ht_median (04/20/2022)

#path to working directories
path_to_nonimp_data <- "~/Library/CloudStorage/Box-Box/Asian_Americans_dementia_data/generation x education/Paper/2022-04-11-/"
path_to_imp_data <- "~/Library/CloudStorage/Box-Box/Asian_Americans_dementia_data/generation x education/Paper/2022-04-11-/Imputed_data/"
path_to_output <- "~/Library/CloudStorage/Box-Box/Asian_Americans_dementia_data/generation x education/Paper/2022-04-11-/Table1/"

#load packages
library(haven)
library(tidyverse)
library(data.table)
library(skimr)
library(rlang)
library(openxlsx)

options(scipen = 999, digits=6)

#---- Importing Datsets ----
#Importing dataset using "haven" package
data_pre_im <- read_sas(paste0(path_to_nonimp_data,"gen_edu_data_for_mi_2022_04_18.sas7bdat"))
load(paste0(path_to_imp_data, "gen_edu_analysis_2022_04_18.R"))

#Making copies of dataset w/ clearer names
data_post_im <- gen_edu_analysis
table(data_pre_im$education_rev, exclude=NULL)
table(data_post_im$education_rev, exclude = NULL)/20

#making a list of variables to keep
colnames(data_pre_im)
colnames(data_post_im)

# add generalhealth, sr_stroke, sr_diabetes, sr_hyp, and ehr_ht_median (04/18/2022)
keepvars <- c("subjid", "survey_age", "female", "asian", "education_rev","education_cat", 
            "income_pp", "usaborn_rev", "income", "sizeofhh", "ethnicity_rev",
            "usabornfather_rev", "usabornmother_rev", "maritalstatus", "smoking_status",
            "college_edu", "survey_language", 
            "generalhealth", "sr_stroke", "sr_diabetes", "sr_hyp", "ehr_ht_median",  
            "main_dem_v1_end_type", "main_dem_v1_end_age",
            "main_dem_v1_fu_time")

keepvars_1 <- c("subjid", "survey_age", "female", "asian", "education_rev","education_cat", 
            "income_pp", "usaborn_rev", "income", "sizeofhh", "ethnicity_rev",
            "usabornfather_rev", "usabornmother_rev", "maritalstatus", "smoking_status",
            "college_edu", "survey_language",
            "generalhealth", "sr_stroke", "sr_diabetes", "sr_hyp", "ehr_ht_median",
            "main_dem_v1_end_type", "main_dem_v1_end_age",
            "main_dem_v1_fu_time", "imp")

#Drop step - subsetting dataset to those selected values
#Note: imputed dataset includes 1 additional variable, 'imp' that has the 
#imputation number.
data_pre_im <- data_pre_im %>% select(all_of(keepvars))
data_post_im <- data_post_im %>% select(all_of(keepvars_1))

#making a list of categorical variables to run the loop over
catvars <- c("female", "ethnicity_rev", "education_rev", "education_cat", "college_edu",
              "usaborn_rev", "usabornfather_rev", "usabornmother_rev", 
             "income", "sizeofhh", "maritalstatus", "smoking_status",
              "survey_language",
             "generalhealth", "sr_stroke", "sr_diabetes", "sr_hyp",
             "main_dem_v1_end_type")

#----Asian vs White Table 1, nonimputed dataset - Categorical Variables----
#Get n's for the whole Asian ethnicity 
#Note: need to add an additional row if variable you are stratifying on has
#missingness
T1results_cat<-matrix(nrow=1, ncol=2) 
T1results_cat[1,]<- c("Race/ethnicity total",
                      table(data_pre_im$asian, exclude = NULL))

for (i in 1:length(catvars)){
  tab.to.add<-table(eval(rlang::parse_expr(paste0("data_pre_im$",catvars[i]))),
                    data_pre_im$asian, exclude=NULL)
  labs<-paste(catvars[i],as.character(rownames(tab.to.add)))
  T1results_cat<-rbind(T1results_cat, c(paste(catvars[i]), rep(NA,1))) 
  T1results_cat<-rbind(T1results_cat,cbind(labs, tab.to.add))
}#Notes: parse_expr() spits out the string as an expression. Must use eval() fxn
#in order to use the expression (if eval() removed, will cause an error)

colnames(T1results_cat)<-c("Variablename", "Asian") 
rownames(T1results_cat)<-NULL
T1results_cat<-as.data.frame(T1results_cat)

#Get %'s by race/ethnicity
T1results_prop<-matrix(nrow=1, ncol=2) 
T1results_prop[1,]<- c("Race/Ethnicity total",table(
  data_pre_im$asian)/nrow(data_pre_im))

#Getting the denominators for the percentage calculations
Racethmargins<-as.numeric(table(data_pre_im$asian))

for (i in 1:length(catvars)){
  tab.to.add<-t(t(table(eval(rlang::parse_expr(paste0("data_pre_im$",catvars[i])))
                        ,data_pre_im$asian, exclude=NULL))/Racethmargins)
  labs<-paste(catvars[i],as.character(rownames(tab.to.add)))
  T1results_prop<-rbind(T1results_prop, c(paste(catvars[i]),rep(NA,1))) 
  T1results_prop<-rbind(T1results_prop,cbind(labs, tab.to.add))
}

colnames(T1results_prop)<-c("Variablename", "Asian_prop") 
rownames(T1results_prop)<-NULL
T1results_prop<-as.data.frame(T1results_prop)
T1results_prop$Asian_prop <- paste0("(", 
                                    round(as.numeric(T1results_prop$Asian_prop)*100, 1),
                                    ")") #percentage value (03/30/2022)

#merge n and % results
T1results_cat<-left_join(T1results_cat, T1results_prop, by="Variablename")
T1results_cat<-T1results_cat[,c("Variablename", "Asian", "Asian_prop")]

#----Asian Table 1, nonimputed datset - continuous----
contvars<-c("survey_age", "income_pp", "ehr_ht_median","main_dem_v1_fu_time")
T1results_cont<-matrix(nrow=0, ncol=2) 
colnames(T1results_cont)<-c("Variablename", "Asian")

for (i in 1:length(contvars)){
  
  tab.to.add<-tapply(eval(parse_expr(paste0("data_pre_im$",contvars[i]))), 
                     data_pre_im$asian, mean, na.rm=T)
  T1results_cont<-rbind(T1results_cont, c(paste0(contvars[i],"_mean"),tab.to.add)) 
  
  tab.to.add2<-tapply(eval(parse_expr(paste0("data_pre_im$",contvars[i]))), 
                      data_pre_im$asian, sd, na.rm=T)
  T1results_cont<-rbind(T1results_cont, c(paste0(contvars[i],"_SD"),tab.to.add2)) 
  
  if (sum(is.na(eval(parse_expr(paste0("data_pre_im$",contvars[i]))))) >0){
    tab.to.add3<-table(is.na(eval(parse_expr(paste0("data_pre_im$",contvars[i]))))
                       ,data_pre_im$asian, exclude=NULL)["TRUE",]
    T1results_cont<-rbind(T1results_cont, c(paste0(contvars[i],"_missing"),tab.to.add3))
    
  }
  
  tab.to.add4<-tapply(eval(parse_expr(paste0("data_pre_im$",contvars[i]))), 
                      data_pre_im$asian, median, na.rm=T)
  T1results_cont<-rbind(T1results_cont, c(paste0(contvars[i],"_median"),tab.to.add4)) 
  
  tab.to.add5<-tapply(eval(parse_expr(paste0("data_pre_im$",contvars[i]))), 
                      data_pre_im$asian, quantile, probs=0.25, na.rm=T)
  T1results_cont<-rbind(T1results_cont, c(paste0(contvars[i],"_q1"),tab.to.add5)) 

  tab.to.add6<-tapply(eval(parse_expr(paste0("data_pre_im$",contvars[i]))), 
                      data_pre_im$asian, quantile, probs=0.75, na.rm=T)
  T1results_cont<-rbind(T1results_cont, c(paste0(contvars[i],"_q3"),tab.to.add6)) 
  
}
T1results_cont<-data.frame(T1results_cont)

#----Asian Table 1, imputed datset - categorical ----
#Getting values for the imputed dataset; there should be no missingness!
T1results_cat_impute<-matrix(nrow=1, ncol=2) 
T1results_cat_impute[1,]<- c("Race/ethnicity total",
                             table(data_post_im$asian, exclude = NULL)/20)

for (i in 1:length(catvars)){
  tab.to.add<-table(eval(parse_expr(paste0("data_post_im$",catvars[i]))),
                    data_post_im$asian, exclude=NULL)/20
  labs<-paste(catvars[i],as.character(rownames(tab.to.add)))
  T1results_cat_impute<-rbind(T1results_cat_impute, c(paste(catvars[i]), rep(NA,1))) 
  T1results_cat_impute<-rbind(T1results_cat_impute,cbind(labs, tab.to.add))
}
#Notes: parse_expr() spits out the string as an expression. Must use eval() fxn
#in order to use the expression (if eval() removed, will cause an error)

colnames(T1results_cat_impute)<-c("Variablename", "Asian") 
rownames(T1results_cat_impute)<-NULL
T1results_cat_impute<-as.data.frame(T1results_cat_impute)

#Get %'s by race/ethnicity
T1results_prop_impute<-matrix(nrow=1, ncol=2) 
T1results_prop_impute[1,]<- c("Race/Ethnicity total",table(
  data_post_im$asian)/nrow(data_post_im))

#Getting the denominators for the percentage calculations
Racethmargins<-as.numeric(table(data_post_im$asian))

for (i in 1:length(catvars)){
  tab.to.add<-t(t(table(eval(parse_expr(paste0("data_post_im$",catvars[i])))
                        ,data_post_im$asian, exclude=NULL))/Racethmargins)
  labs<-paste(catvars[i],as.character(rownames(tab.to.add)))
  T1results_prop_impute<-rbind(T1results_prop_impute, c(paste(catvars[i]),rep(NA,1))) 
  T1results_prop_impute<-rbind(T1results_prop_impute,cbind(labs, tab.to.add))
}

colnames(T1results_prop_impute)<-c("Variablename", "Asian_prop") 
rownames(T1results_prop_impute)<-NULL
T1results_prop_impute<-as.data.frame(T1results_prop_impute)
T1results_prop_impute$Asian_prop <- paste0("(", 
                                           round(as.numeric(T1results_prop_impute$Asian_prop)*100, 1),
                                           ")") #percentage value (03/30/2022)

#merge n and % results
T1results_cat_impute<-left_join(T1results_cat_impute, 
                                T1results_prop_impute, by="Variablename")
T1results_cat_impute<-T1results_cat_impute[,c("Variablename", "Asian", "Asian_prop")]

#----Asian subgroups Table 1, non-imputed dataset - categorical ----
#the same catvars as the ones for the whole sample
# catvars <- c("female", "ethnicity_rev", "education_rev", "education_cat", "college_edu",
#              "usaborn_rev", "usabornfather_rev", "usabornmother_rev", 
#              "income", "sizeofhh", "maritalstatus", "smoking_status",
#              "survey_language","main_dem_v1_end_type")

#making a list of categorical variables to run the loop over

#Get n's for all race/ethnicities vs white ethnicity 
#Note: need to add an additional row if variable you are stratifying on has
#missingness
table(data_pre_im$ethnicity_rev, exclude = NULL)

T1results_cat_substr<-matrix(nrow=1, ncol=4) 
T1results_cat_substr[1,]<- c("Race/ethnicity total",
                             table(data_pre_im$ethnicity_rev, exclude = NULL))

for (i in 1:length(catvars)){
  tab.to.add<-table(eval(parse_expr(paste0("data_pre_im$",catvars[i]))),
                    data_pre_im$ethnicity_rev, exclude=NULL)
  labs<-paste(catvars[i],as.character(rownames(tab.to.add)))
  T1results_cat_substr<-rbind(T1results_cat_substr, c(paste(catvars[i]), rep(NA,3))) 
  T1results_cat_substr<-rbind(T1results_cat_substr, cbind(labs, tab.to.add))
}#Notes: parse_expr() spits out the string as an expression. Must use eval() fxn
#in order to use the expression (if eval() removed, will cause an error)

colnames(T1results_cat_substr)<-c("Variablename", "Chinese", "Japanese", "Filipino") 
rownames(T1results_cat_substr)<-NULL
T1results_cat_substr<-as.data.frame(T1results_cat_substr)

#Get %'s by race/ethnicity
T1results_prop_substr<-matrix(nrow=1, ncol=4) 
T1results_prop_substr[1,]<- c("Race/Ethnicity total",table(
  data_pre_im$ethnicity_rev)/nrow(data_pre_im))

#Getting the denominators for the percentage calculations
Racethmargins<-as.numeric(table(data_pre_im$ethnicity_rev))

for (i in 1:length(catvars)){
  tab.to.add<-t(t(table(eval(parse_expr(paste0("data_pre_im$",catvars[i])))
                        ,data_pre_im$ethnicity_rev, exclude=NULL))/Racethmargins)
  labs<-paste(catvars[i],as.character(rownames(tab.to.add)))
  T1results_prop_substr<-rbind(T1results_prop_substr, c(paste(catvars[i]),rep(NA,3))) 
  T1results_prop_substr<-rbind(T1results_prop_substr, cbind(labs, tab.to.add))
}

colnames(T1results_prop_substr)<-c("Variablename", "Chinese_prop", "Japanese_prop","Filipino_prop") 
rownames(T1results_prop_substr)<-NULL
T1results_prop_substr<-as.data.frame(T1results_prop_substr)
T1results_prop_substr$Chinese_prop <- paste0("(", 
                                             round(as.numeric(T1results_prop_substr$Chinese_prop)*100, 1),
                                             ")") #percentage value (03/30/2022)
T1results_prop_substr$Japanese_prop <- paste0("(", 
                                              round(as.numeric(T1results_prop_substr$Japanese_prop)*100, 1),
                                              ")") #percentage value (03/30/2022)
T1results_prop_substr$Filipino_prop <- paste0("(", 
                                              round(as.numeric(T1results_prop_substr$Filipino_prop)*100, 1),
                                              ")") #percentage value (03/30/2022)

#merge n and % results
T1results_cat_substr<-left_join(T1results_cat_substr, T1results_prop_substr, by="Variablename")
T1results_cat_substr<-T1results_cat_substr[
  ,c("Variablename", "Chinese", "Chinese_prop",
     "Japanese","Japanese_prop", 
     "Filipino", "Filipino_prop")]

#----Asian subgroups Table 1, nonimputed dataset - continuous----
#Now adding continuous variables
#contvars<-c("survey_age", "income_pp","main_dem_v1_fu_time")
T1results_cont_substr<-matrix(nrow=0, ncol=4) 
colnames(T1results_cont_substr)<-c("Variablename", "Chinese", "Japanese","Filipino") 

for (i in 1:length(contvars)){
  
  tab.to.add<-tapply(eval(parse_expr(paste0("data_pre_im$",contvars[i]))), 
                     data_pre_im$ethnicity_rev, mean, na.rm=T)
  T1results_cont_substr<-rbind(T1results_cont_substr, c(paste0(contvars[i],"_mean"),tab.to.add)) 
  
  tab.to.add2<-tapply(eval(parse_expr(paste0("data_pre_im$",contvars[i]))), 
                      data_pre_im$ethnicity_rev, sd, na.rm=T)
  T1results_cont_substr<-rbind(T1results_cont_substr, c(paste0(contvars[i],"_SD"),tab.to.add2)) 
  
  if (sum(is.na(eval(parse_expr(paste0("data_pre_im$",contvars[i]))))) >0){
    tab.to.add3<-table(is.na(eval(parse_expr(paste0("data_pre_im$",contvars[i]))))
                       ,data_pre_im$ethnicity_rev, exclude=NULL)["TRUE",]
    T1results_cont_substr<-rbind(T1results_cont_substr, c(paste0(contvars[i],"_missing"),tab.to.add3)) 
  }
  tab.to.add4<-tapply(eval(parse_expr(paste0("data_pre_im$",contvars[i]))), 
                      data_pre_im$ethnicity_rev, median, na.rm=T)
  T1results_cont_substr<-rbind(T1results_cont_substr, c(paste0(contvars[i],"_median"),tab.to.add4)) 
  
  tab.to.add5<-tapply(eval(parse_expr(paste0("data_pre_im$",contvars[i]))), 
                      data_pre_im$ethnicity_rev, quantile, probs=0.25, na.rm=T)
  T1results_cont_substr<-rbind(T1results_cont_substr, c(paste0(contvars[i],"_q1"),tab.to.add5)) 
  
  tab.to.add6<-tapply(eval(parse_expr(paste0("data_pre_im$",contvars[i]))), 
                      data_pre_im$ethnicity_rev, quantile, probs=0.75, na.rm=T)
  T1results_cont_substr<-rbind(T1results_cont_substr, c(paste0(contvars[i],"_q3"),tab.to.add6)) 
}
T1results_cont_substr<-data.frame(T1results_cont_substr)

#----Asian subgroups Table 1, imputed dataset - categorical----
#Getting values for the imputed dataset; there should be no missingness!
T1results_cat_impute_substr<-matrix(nrow=1, ncol=4) 
T1results_cat_impute_substr[1,]<- c("Race/ethnicity total",
                                    table(data_post_im$ethnicity_rev, exclude = NULL)/20)

for (i in 1:length(catvars)){
  tab.to.add<-table(eval(parse_expr(paste0("data_post_im$",catvars[i]))),
                    data_post_im$ethnicity_rev, exclude=NULL)/20
  labs<-paste(catvars[i],as.character(rownames(tab.to.add)))
  T1results_cat_impute_substr<-rbind(T1results_cat_impute_substr, c(paste(catvars[i]), rep(NA,3))) 
  T1results_cat_impute_substr<-rbind(T1results_cat_impute_substr,cbind(labs, tab.to.add))
}
#Notes: parse_expr() spits out the string as an expression. Must use eval() fxn
#in order to use the expression (if eval() removed, will cause an error).
#Number of repeated values for rowbind should equal number of strata you have

colnames(T1results_cat_impute_substr)<-c("Variablename", "Chinese", "Japanese","Filipino") 
rownames(T1results_cat_impute_substr)<-NULL
T1results_cat_impute_substr<-as.data.frame(T1results_cat_impute_substr)

#Get %'s by race/ethnicity
T1results_prop_impute_substr<-matrix(nrow=1, ncol=4) 
T1results_prop_impute_substr[1,]<- c("Race/Ethnicity total",table(
  data_post_im$ethnicity_rev)/nrow(data_post_im))

#Getting the denominators for the percentage calculations
Racethmargins<-as.numeric(table(data_post_im$ethnicity_rev))

for (i in 1:length(catvars)){
  tab.to.add<-t(t(table(eval(parse_expr(paste0("data_post_im$",catvars[i])))
                        ,data_post_im$ethnicity_rev, exclude=NULL))/Racethmargins)
  labs<-paste(catvars[i],as.character(rownames(tab.to.add)))
  T1results_prop_impute_substr<-rbind(T1results_prop_impute_substr, c(paste(catvars[i]),rep(NA,3))) 
  T1results_prop_impute_substr<-rbind(T1results_prop_impute_substr,cbind(labs, tab.to.add))
}

colnames(T1results_prop_impute_substr)<-c("Variablename", "Chinese_prop", 
                                          "Japanese_prop","Filipino_prop")  
rownames(T1results_prop_impute_substr)<-NULL
T1results_prop_impute_substr<-as.data.frame(T1results_prop_impute_substr)
T1results_prop_impute_substr$Chinese_prop <- paste0("(", 
                                             round(as.numeric(T1results_prop_impute_substr$Chinese_prop)*100, 1),
                                             ")") #percentage value (03/30/2022)
T1results_prop_impute_substr$Japanese_prop <- paste0("(", 
                                              round(as.numeric(T1results_prop_impute_substr$Japanese_prop)*100, 1),
                                              ")") #percentage value (03/30/2022)
T1results_prop_impute_substr$Filipino_prop <- paste0("(", 
                                              round(as.numeric(T1results_prop_impute_substr$Filipino_prop)*100, 1),
                                              ")") #percentage value (03/30/2022)

#merge n and % results
T1results_cat_impute_substr<-left_join(T1results_cat_impute_substr, 
                                       T1results_prop_impute_substr, by="Variablename")
T1results_cat_impute_substr<-T1results_cat_impute_substr[
  ,c("Variablename", "Chinese", "Chinese_prop",
     "Japanese","Japanese_prop", 
     "Filipino", "Filipino_prop")]

#----Asian subgroups, imputed dataset - continuous w/ Rubin's----
#Rubin's rules for continuous values# (ASK!! Is this Rubin's rules?) 02/16/2022
#Pivoting long datasets to wide datasets for each of the variables

cont_results <- 
  tibble("var" = 
           rep(c("main_dem_v1_fu_time", "survey_age", "income_pp", "ehr_ht_median"), 
               each = 5), 
         "stat" = rep(c("mean", "SD", "median", "q1", "q3"), 4), 
         "Asian" = 0, "2" = 0, "3" = 0, "5" = 0)

for(var in c("main_dem_v1_fu_time", "survey_age", "income_pp", "ehr_ht_median")){
  # subset_test <- data_post_im %>% 
  #   dplyr::select("subjid", "imp", "asian", "ethnicity_rev", "main_dem_v1_fu_time")
  
  subset <- data_post_im %>% 
    dplyr::select("subjid", "imp", "asian", "ethnicity_rev", all_of(var))

  # AA_subset_test <- subset_test %>% filter(asian == 1) %>% 
  #   dplyr::select(c("subjid", "imp", "main_dem_v1_fu_time")) %>% 
  #   pivot_wider(id_cols = subjid, names_from = imp, values_from = main_dem_v1_fu_time)
    
  AA_subset <- subset %>% filter(asian == 1) %>% 
    dplyr::select(c("subjid", "imp", all_of(var))) %>% 
    pivot_wider(id_cols = subjid, names_from = imp, values_from = var)
  
  AA_data <- as.matrix(AA_subset[, 2:21])
  AA_imp_mean = rowMeans(AA_data)
  
  # AA_data_test <- as.matrix(AA_subset_test[, 2:21])
  # AA_imp_mean_test = rowMeans(AA_data_test)
  
  cont_results[which(cont_results$var == var & cont_results$stat == "mean"), 
               "Asian"] <- mean(AA_imp_mean)
  cont_results[which(cont_results$var == var & cont_results$stat == "SD"), 
               "Asian"] <- sd(AA_imp_mean)
  cont_results[which(cont_results$var == var & cont_results$stat == "median"), 
               "Asian"] <- median(AA_imp_mean)
  cont_results[which(cont_results$var == var & cont_results$stat == "q1"), 
               "Asian"] <- quantile(AA_imp_mean, probs = 0.25)
  cont_results[which(cont_results$var == var & cont_results$stat == "q3"), 
               "Asian"] <- quantile(AA_imp_mean, probs = 0.75)
  
  
  for(group in names(table(subset$ethnicity_rev))){
    subgroup <- subset %>% filter(ethnicity_rev == group) %>% 
      dplyr::select(c("subjid", "imp", all_of(var))) %>% 
      pivot_wider(id_cols = subjid, names_from = imp, values_from = var)
    
    subgroup_data <- as.matrix(subgroup[, 2:21])
    
    subgroup_imp_mean = rowMeans(subgroup_data)
    cont_results[which(cont_results$var == var & cont_results$stat == "mean"), 
                 group] <- mean(subgroup_imp_mean)
    cont_results[which(cont_results$var == var & cont_results$stat == "SD"), 
                 group] <- sd(subgroup_imp_mean)
    cont_results[which(cont_results$var == var & cont_results$stat == "median"), 
                 group] <- median(subgroup_imp_mean)
    cont_results[which(cont_results$var == var & cont_results$stat == "q1"), 
                 group] <- quantile(subgroup_imp_mean, probs = 0.25)
    cont_results[which(cont_results$var == var & cont_results$stat == "q3"), 
                 group] <- quantile(subgroup_imp_mean, probs = 0.75)
  }
}
colnames(cont_results)<-c("var","stat","Asian","Chinese", 
                          "Japanese","Filipino")

cont_results$concat<-paste(cont_results$var, sep = "_", cont_results$stat)
#----Export results to a raw Excel----
t1_res<-list(catvars=T1results_cat, 
             contvars=T1results_cont, 
             catvars_impute=T1results_cat_impute,
             catvars_substr = T1results_cat_substr,
             contvars_substr = T1results_cont_substr,
             catvars_impute_substr = T1results_cat_impute_substr,
             contvars_impute_all = cont_results)

write.xlsx(t1_res,
           file = paste0(path_to_output, "gen_edu_Table1_raw_2022_04_20_v2.xlsx"),
           overwrite = TRUE)