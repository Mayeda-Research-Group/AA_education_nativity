# input
# gen_edu_data_for_mi_2022_12_06.sas7bdat
# gen_edu_analysis_2022_12_06.R
# R4.1.3

# objective of this file
# Table1 for stacked imputed data, stratified by education using `college_edu` 

#change a format for the paper manuscript 
#add generalhealth, sr_stroke, sr_diabetes, sr_hyp, and ehr_ht_median 
# add variables: employment_retired 
#stratify by `college_edu` using `data_post_im` 
#collapse size of HH to alone/two/3+ 
#collapse self-rated health to excellent/very good, good, and fair/poor 

#path to working directories
path_to_nonimp_data <- "~/Library/CloudStorage/Box-Box/Asian_Americans_dementia_data/generation x education/Paper/Revise_1/"
path_to_imp_data <- "~/Library/CloudStorage/Box-Box/Asian_Americans_dementia_data/generation x education/Paper/Revise_1/Imputed_data/"
path_to_output <- "~/Library/CloudStorage/Box-Box/Asian_Americans_dementia_data/generation x education/Paper/Revise_1/Table1/"

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
data_pre_im <- read_sas(paste0(path_to_nonimp_data,"gen_edu_data_for_mi_2022_12_06.sas7bdat"))
load(paste0(path_to_imp_data, "gen_edu_analysis_2022_12_06.R"))

#Making copies of dataset w/ clearer names
data_post_im <- gen_edu_analysis
# table(data_pre_im$education_rev, exclude=NULL)
table(data_post_im$education_rev, exclude = NULL)/20

#collapsing sizeofhh and generalhealth 
#data_pre_im (pre-imputation data)
table(data_pre_im$sizeofhh)
data_pre_im <- data_pre_im %>% mutate(
  sizeofhh_coll = case_when(
    sizeofhh==1 ~ "Living_alone",
    sizeofhh==2 ~ "Two",
    sizeofhh==3|sizeofhh==4|sizeofhh==5 ~ "Three_or_more",
  ))
xtabs(~ sizeofhh + sizeofhh_coll, data = data_pre_im,
      addNA = T)

table(data_pre_im$generalhealth)
data_pre_im <- data_pre_im %>% mutate(
  generalhealth_coll = case_when(
    generalhealth==1|generalhealth==2 ~ "Excellent/very_good",
    generalhealth==3 ~ "Good",
    generalhealth==4|generalhealth==5 ~ "Fair/poor",
  ))
xtabs(~ generalhealth + generalhealth_coll, data = data_pre_im,
      addNA = T)

#binary for age 65 or older 
data_pre_im <- data_pre_im %>% mutate(
  age65_or_older = case_when(
    survey_age >= 65 ~ "Yes",
    survey_age < 65 ~ "No"
  ))
xtabs(~ age65_or_older, data = data_pre_im, addNA = T) # No/Yes = 4043/10706

#data_post_im (post-imputation data)
table(data_post_im$sizeofhh)
data_post_im <- data_post_im %>% mutate(
  sizeofhh_coll = case_when(
    sizeofhh==1 ~ "Living_alone",
    sizeofhh==2 ~ "Two",
    sizeofhh==3|sizeofhh==4|sizeofhh==5 ~ "Three_or_more",
  ))
xtabs(~ sizeofhh + sizeofhh_coll, data = data_post_im,
      addNA = T)

table(data_post_im$generalhealth)
data_post_im <- data_post_im %>% mutate(
  generalhealth_coll = case_when(
    generalhealth==1|generalhealth==2 ~ "Excellent/very_good",
    generalhealth==3 ~ "Good",
    generalhealth==4|generalhealth==5 ~ "Fair/poor",
  ))
xtabs(~ generalhealth + generalhealth_coll, data = data_post_im,
      addNA = T)

#binary for age 65 or older 
data_post_im <- data_post_im %>% mutate(
  age65_or_older = case_when(
    survey_age >= 65 ~ "Yes",
    survey_age < 65 ~ "No"
  ))
xtabs(~ age65_or_older, data = data_post_im, addNA = T) # No/Yes = 80860/214120

#making a list of variables to keep
colnames(data_pre_im)
colnames(data_post_im)
keepvars <- c("subjid", "survey_age", "female", "asian", "education_rev","education_cat", 
              "income_pp", "usaborn_rev", "income", "sizeofhh", "ethnicity_rev",
              "usabornfather_rev", "usabornmother_rev", "maritalstatus", "smoking_status",
              "college_edu", "survey_language", 
              "generalhealth", "sr_stroke", "sr_diabetes", "sr_hyp", "ehr_ht_median",  
              "sizeofhh_coll", "generalhealth_coll", 
              "employment_retired", "age65_or_older", 
              "main_dem_v1_end_type", "main_dem_v1_end_age",
              "main_dem_v1_fu_time")

keepvars_1 <- c("subjid", "survey_age", "female", "asian", "education_rev","education_cat", 
            "income_pp", "usaborn_rev", "income", "sizeofhh", "ethnicity_rev",
            "usabornfather_rev", "usabornmother_rev", "maritalstatus", "smoking_status",
            "college_edu", "survey_language",
            "generalhealth", "sr_stroke", "sr_diabetes", "sr_hyp", "ehr_ht_median",
            "sizeofhh_coll", "generalhealth_coll", 
            "employment_retired", "age65_or_older", 
            "main_dem_v1_end_type", "main_dem_v1_end_age",
            "main_dem_v1_fu_time", "imp")

#Drop step - subsetting dataset to those selected values
#Note: imputed dataset includes 1 additional variable, 'imp' that has the 
#imputation number.
data_pre_im <- data_pre_im %>% select(all_of(keepvars)) %>% as.data.table()
data_post_im <- data_post_im %>% select(all_of(keepvars_1)) %>% as.data.table()

# change the variable class of `usa_born`: number -> character
data_pre_im$college_edu <- as.character(data_pre_im$college_edu)
data_pre_im[is.na(college_edu), college_edu:="missing"]
table(data_pre_im$college_edu)

#making a list of categorical variables to run the loop over
catvars <- c("female", "ethnicity_rev", "education_rev", "education_cat", "college_edu",
              "usaborn_rev", "usabornfather_rev", "usabornmother_rev", 
             "income", "sizeofhh", "maritalstatus", "smoking_status",
              "survey_language",
             "generalhealth", "sr_stroke", "sr_diabetes", "sr_hyp",
             "sizeofhh_coll", "generalhealth_coll", 
             "employment_retired", "age65_or_older", 
             "main_dem_v1_end_type")

#----Asian vs White Table 1, nonimputed dataset - Categorical Variables----
#Get n's for the whole Asian ethnicity 
#Note: need to add an additional row if variable you are stratifying on has
#missingness
T1results_cat<-matrix(nrow=1, ncol=4) 
T1results_cat[1,]<- c("Race/ethnicity total",
                      table(data_pre_im$college_edu, exclude = NULL))
# table(data_pre_im$college_edu, exclude = NULL) sanity check 

for (i in 1:length(catvars)){
  tab.to.add<-table(eval(rlang::parse_expr(paste0("data_pre_im$",catvars[i]))),
                    data_pre_im$college_edu, exclude=NULL)
  labs<-paste(catvars[i],as.character(rownames(tab.to.add)))
  T1results_cat<-rbind(T1results_cat, c(paste(catvars[i]), rep(NA,3))) 
  T1results_cat<-rbind(T1results_cat,cbind(labs, tab.to.add))
}#Notes: parse_expr() spits out the string as an expression. Must use eval() fxn
#in order to use the expression (if eval() removed, will cause an error)

T1results_cat
colnames(T1results_cat)<-c("Variablename", "Asian_less_than_college", "Asian_college_or_more", "Asian_education_NA") 
rownames(T1results_cat)<-NULL
T1results_cat<-as.data.frame(T1results_cat)
T1results_cat

#Get %'s by race/ethnicity
T1results_prop<-matrix(nrow=1, ncol=4) 
T1results_prop[1,]<- c("Race/Ethnicity total",table(
  data_pre_im$college_edu, exclude = NULL)/nrow(data_pre_im))

#Getting the denominators for the percentage calculations
Racethmargins <- as.numeric(table(data_pre_im$college_edu, exclude = NULL))

for (i in 1:length(catvars)){
  tab.to.add<-t(t(table(eval(rlang::parse_expr(paste0("data_pre_im$",catvars[i])))
                        ,data_pre_im$college_edu, exclude=NULL))/Racethmargins)
  labs<-paste(catvars[i],as.character(rownames(tab.to.add)))
  T1results_prop<-rbind(T1results_prop, c(paste(catvars[i]),rep(NA,3))) 
  T1results_prop<-rbind(T1results_prop,cbind(labs, tab.to.add))
}

T1results_prop
colnames(T1results_prop)<-c("Variablename", "Asian_less_than_college_prop", "Asian_college_or_more_prop", "Asian_education_NA_prop") 
rownames(T1results_prop)<-NULL
T1results_prop<-as.data.frame(T1results_prop)
T1results_prop$Asian_less_than_college_prop <- paste0("(", 
                                    round(as.numeric(T1results_prop$Asian_less_than_college_prop)*100, 1),
                                    ")") 
T1results_prop$Asian_college_or_more_prop <- paste0("(", 
                                                 round(as.numeric(T1results_prop$Asian_college_or_more_prop)*100, 1),
                                                 ")") 
T1results_prop$Asian_education_NA_prop <- paste0("(", 
                                                 round(as.numeric(T1results_prop$Asian_education_NA_prop)*100, 1),
                                                 ")") 

#merge n and % results
T1results_cat <- left_join(T1results_cat, T1results_prop, by="Variablename")
T1results_cat <- T1results_cat[,c("Variablename",
                                "Asian_less_than_college", "Asian_less_than_college_prop",
                                "Asian_college_or_more", "Asian_college_or_more_prop",
                                "Asian_education_NA", "Asian_education_NA_prop")]
T1results_cat

#----Asian Table 1, nonimputed datset - continuous----
contvars<-c("survey_age", "income_pp", "ehr_ht_median","main_dem_v1_fu_time")
T1results_cont<-matrix(nrow=0, ncol=4) 
colnames(T1results_cont)<-c("Variablename", "Asian_less_than_college", "Asian_college_or_more", "Asian_education_NA")

for (i in 1:length(contvars)){
  
  tab.to.add<-tapply(eval(parse_expr(paste0("data_pre_im$",contvars[i]))), 
                     data_pre_im$college_edu, mean, na.rm=T)
  T1results_cont<-rbind(T1results_cont, c(paste0(contvars[i],"_mean"),tab.to.add)) 
  
  tab.to.add2<-tapply(eval(parse_expr(paste0("data_pre_im$",contvars[i]))), 
                      data_pre_im$college_edu, sd, na.rm=T)
  T1results_cont<-rbind(T1results_cont, c(paste0(contvars[i],"_SD"),tab.to.add2)) 
  
  if (sum(is.na(eval(parse_expr(paste0("data_pre_im$",contvars[i]))))) >0){
    # table((is.na(data_pre_im$ehr_ht_median)),data_pre_im$college_edu) 
    tab.to.add3<-table(is.na(eval(parse_expr(paste0("data_pre_im$",contvars[i]))))
                       ,data_pre_im$college_edu, exclude=NULL)["TRUE",]
    T1results_cont<-rbind(T1results_cont, c(paste0(contvars[i],"_missing"),tab.to.add3))
    
  }
  
  tab.to.add4<-tapply(eval(parse_expr(paste0("data_pre_im$",contvars[i]))), 
                      data_pre_im$college_edu, median, na.rm=T)
  T1results_cont<-rbind(T1results_cont, c(paste0(contvars[i],"_median"),tab.to.add4)) 
  
  tab.to.add5<-tapply(eval(parse_expr(paste0("data_pre_im$",contvars[i]))), 
                      data_pre_im$college_edu, quantile, probs=0.25, na.rm=T)
  T1results_cont<-rbind(T1results_cont, c(paste0(contvars[i],"_q1"),tab.to.add5)) 
  
  tab.to.add6<-tapply(eval(parse_expr(paste0("data_pre_im$",contvars[i]))), 
                      data_pre_im$college_edu, quantile, probs=0.75, na.rm=T)
  T1results_cont<-rbind(T1results_cont, c(paste0(contvars[i],"_q3"),tab.to.add6)) 
  
}
T1results_cont<-data.frame(T1results_cont)
T1results_cont

#----Asian Table 1, imputed datset - categorical ----
#Getting values for the imputed dataset; there should be no missingness!
T1results_cat_impute<-matrix(nrow=1, ncol=3) 
T1results_cat_impute[1,]<- c("Race/ethnicity total",
                             table(data_post_im$college_edu, exclude = NULL)/20)

for (i in 1:length(catvars)){
  tab.to.add<-table(eval(parse_expr(paste0("data_post_im$",catvars[i]))),
                    data_post_im$college_edu, exclude=NULL)/20
  labs<-paste(catvars[i],as.character(rownames(tab.to.add)))
  T1results_cat_impute<-rbind(T1results_cat_impute, c(paste(catvars[i]), rep(NA,2))) 
  T1results_cat_impute<-rbind(T1results_cat_impute,cbind(labs, tab.to.add))
}
#Notes: parse_expr() spits out the string as an expression. Must use eval() fxn
#in order to use the expression (if eval() removed, will cause an error)

T1results_cat_impute
colnames(T1results_cat_impute)<-c("Variablename", "Asian_less_than_college", "Asian_college_or_more") 
rownames(T1results_cat_impute)<-NULL
T1results_cat_impute<-as.data.frame(T1results_cat_impute)

#Get %'s by race/ethnicity
T1results_prop_impute<-matrix(nrow=1, ncol=3) 
T1results_prop_impute[1,]<- c("Race/Ethnicity total",table(
  data_post_im$college_edu)/nrow(data_post_im))

#Getting the denominators for the percentage calculations
Racethmargins<-as.numeric(table(data_post_im$college_edu))

for (i in 1:length(catvars)){
  tab.to.add<-t(t(table(eval(parse_expr(paste0("data_post_im$",catvars[i])))
                        ,data_post_im$college_edu, exclude=NULL))/Racethmargins)
  labs<-paste(catvars[i],as.character(rownames(tab.to.add)))
  T1results_prop_impute<-rbind(T1results_prop_impute, c(paste(catvars[i]),rep(NA,2))) 
  T1results_prop_impute<-rbind(T1results_prop_impute,cbind(labs, tab.to.add))
}

T1results_prop_impute
colnames(T1results_prop_impute)<-c("Variablename", "Asian_less_than_college_prop", "Asian_college_or_more_prop") 
rownames(T1results_prop_impute)<-NULL
T1results_prop_impute<-as.data.frame(T1results_prop_impute)
T1results_prop_impute$`Asian_less_than_college_prop` <- paste0("(", 
                                           round(as.numeric(T1results_prop_impute$`Asian_less_than_college_prop`)*100, 1),
                                           ")") 
T1results_prop_impute$`Asian_college_or_more_prop` <- paste0("(", 
                                           round(as.numeric(T1results_prop_impute$`Asian_college_or_more_prop`)*100, 1),
                                           ")") 

#merge n and % results
T1results_cat_impute<-left_join(T1results_cat_impute, 
                                T1results_prop_impute, by="Variablename")
T1results_cat_impute<-T1results_cat_impute[,c("Variablename",
                                              "Asian_less_than_college", "Asian_less_than_college_prop",
                                              "Asian_college_or_more", "Asian_college_or_more_prop")]
T1results_cat_impute


#----Asian subgroups Table 1, non-imputed dataset - categorical ----
#making a list of categorical variables to run the loop over

#Get n's for all race/ethnicities
#Note: need to add an additional row if variable you are stratifying on has
#missingness
table(data_pre_im$ethnicity_rev, exclude = NULL)

# for() for each ethnicity
table_education_cat_str_nonimp <- function(input){
  # dt_input <- data_pre_im[ethnicity_rev==2,]
  if(input == "Chinese"){ #ethnicity_rev == 2
    dt_input <- data_pre_im[ethnicity_rev==2,]
  }else if(input == "Japanese"){ #ethnicity_rev == 3
    dt_input <- data_pre_im[ethnicity_rev==3,]
  }else{ #ethnicity_rev == 5 (Filipino)
    dt_input <- data_pre_im[ethnicity_rev==5,]
  }
  
  tmp_cat_substr<-matrix(nrow=1, ncol=4) 
  tmp_cat_substr[1,]<- c("Race/ethnicity total",
                               table(dt_input$college_edu, exclude = NULL))
  
  for (i in 1:length(catvars)){
    tab.to.add<-table(eval(parse_expr(paste0("dt_input$",catvars[i]))),
                      dt_input$college_edu, exclude=NULL)
    labs<-paste(catvars[i],as.character(rownames(tab.to.add)))
    tmp_cat_substr<-rbind(tmp_cat_substr, c(paste(catvars[i]), rep(NA,3))) 
    tmp_cat_substr<-rbind(tmp_cat_substr, cbind(labs, tab.to.add))
  }
  #Notes: parse_expr() spits out the string as an expression. Must use eval() fxn
  #in order to use the expression (if eval() removed, will cause an error)
  
  colnames(tmp_cat_substr)<-c("Variablename", paste0(input, "_less_than_college"),
                                    paste0(input, "_college_or_more"), paste0(input, "_NA")) 
  rownames(tmp_cat_substr)<-NULL
  tmp_cat_substr<-as.data.frame(tmp_cat_substr)
  
  #Get %'s by race/ethnicity
  tmp_prop_substr<-matrix(nrow=1, ncol=4) 
  tmp_prop_substr[1,]<- c("Race/Ethnicity total",table(
    dt_input$college_edu)/nrow(dt_input))
  
  #Getting the denominators for the percentage calculations
  Racethmargins<-as.numeric(table(dt_input$college_edu))
  
  for (i in 1:length(catvars)){
    tab.to.add<-t(t(table(eval(parse_expr(paste0("dt_input$",catvars[i])))
                          ,dt_input$college_edu, exclude=NULL))/Racethmargins)
    labs<-paste(catvars[i],as.character(rownames(tab.to.add)))
    tmp_prop_substr<-rbind(tmp_prop_substr, c(paste(catvars[i]),rep(NA,3))) 
    tmp_prop_substr<-rbind(tmp_prop_substr, cbind(labs, tab.to.add))
  }
  
  colnames(tmp_prop_substr)<-c("Variablename", paste0(input, "_less_than_college_prop"),
                                     paste0(input, "_college_or_more_prop"), paste0(input, "_NA_prop")) 
  rownames(tmp_prop_substr)<-NULL
  tmp_prop_substr<-as.data.frame(tmp_prop_substr)
  
  #merge n and % results
  tmp_cat_substr<-left_join(tmp_cat_substr, tmp_prop_substr, by="Variablename")
  tmp_cat_substr<-tmp_cat_substr[,c("Variablename", 
                                                paste0(input, "_less_than_college"), paste0(input, "_less_than_college_prop"),
                                                paste0(input, "_college_or_more"), paste0(input, "_college_or_more_prop"), 
                                                paste0(input, "_NA"), paste0(input, "_NA_prop"))]
  
  return(tmp_cat_substr)
  } 

T1results_cat_chinese <- table_education_cat_str_nonimp("Chinese") #86*7 (survey_language Spanish: 4)
T1results_cat_japanese <- table_education_cat_str_nonimp("Japanese") #85*7 (survey_language Spanish: 0)
T1results_cat_filipino <- table_education_cat_str_nonimp("Filipino") #85*7 (survey_language Chinese: 0)
T1results_cat_substr <- 
  left_join(T1results_cat_chinese, T1results_cat_japanese, 
            by="Variablename") %>% left_join(., T1results_cat_filipino,
                                             by="Variablename")
T1results_cat_substr

T1results_cat_substr$Chinese_less_than_college_prop <- paste0("(",
                                                                round(as.numeric(T1results_cat_substr$Chinese_less_than_college_prop)*100, 1),
                                                                ")") 
T1results_cat_substr$Chinese_college_or_more_prop <- paste0("(",
                                                           round(as.numeric(T1results_cat_substr$Chinese_college_or_more_prop)*100, 1),
                                                           ")") 
T1results_cat_substr$Chinese_NA_prop <- paste0("(",
                                                    round(as.numeric(T1results_cat_substr$Chinese_NA_prop)*100, 1),
                                                    ")") 
T1results_cat_substr$Japanese_less_than_college_prop <- paste0("(",
                                                                 round(as.numeric(T1results_cat_substr$Japanese_less_than_college_prop)*100, 1),
                                                                 ")") 
T1results_cat_substr$Japanese_college_or_more_prop <- paste0("(",
                                                            round(as.numeric(T1results_cat_substr$Japanese_college_or_more_prop)*100, 1),
                                                            ")") 
T1results_cat_substr$Japanese_NA_prop <- paste0("(",
                                                     round(as.numeric(T1results_cat_substr$Japanese_NA_prop)*100, 1),
                                                     ")") 
T1results_cat_substr$Filipino_less_than_college_prop <- paste0("(",
                                                                 round(as.numeric(T1results_cat_substr$Filipino_less_than_college_prop)*100, 1),
                                                                 ")") 
T1results_cat_substr$Filipino_college_or_more_prop <- paste0("(",
                                                            round(as.numeric(T1results_cat_substr$Filipino_college_or_more_prop)*100, 1),
                                                            ")") 
T1results_cat_substr$Filipino_NA_prop <- paste0("(",
                                                     round(as.numeric(T1results_cat_substr$Filipino_NA_prop)*100, 1),
                                                     ")") 
T1results_cat_substr

#----Asian subgroups Table 1, nonimputed dataset - continuous----
#Now adding continuous variables
#contvars<-c("survey_age", "income_pp","main_dem_v1_fu_time")

table_education_cont_str_nonimp <- function(input){
  # dt_input <- data_pre_im[ethnicity_rev==2,]
  if(input == "Chinese"){ #ethnicity_rev == 2
    dt_input <- data_pre_im[ethnicity_rev==2,]
  }else if(input == "Japanese"){ #ethnicity_rev == 3
    dt_input <- data_pre_im[ethnicity_rev==3,]
  }else{ #ethnicity_rev == 5 (Filipino)
    dt_input <- data_pre_im[ethnicity_rev==5,]
  }
  
  tmp_cont_substr<-matrix(nrow=0, ncol=4) 
  colnames(tmp_cont_substr)<-c("Variablename", paste0(input, "_less_than_college"),
                                     paste0(input, "_college_or_more"), paste0(input, "_NA")) 
  
  for (i in 1:length(contvars)){
    
    tab.to.add<-tapply(eval(parse_expr(paste0("dt_input$",contvars[i]))), 
                       dt_input$college_edu, mean, na.rm=T)
    tmp_cont_substr<-rbind(tmp_cont_substr, c(paste0(contvars[i],"_mean"),tab.to.add)) 
    
    tab.to.add2<-tapply(eval(parse_expr(paste0("dt_input$",contvars[i]))), 
                        dt_input$college_edu, sd, na.rm=T)
    tmp_cont_substr<-rbind(tmp_cont_substr, c(paste0(contvars[i],"_SD"),tab.to.add2)) 
    
    if (sum(is.na(eval(parse_expr(paste0("dt_input$",contvars[i]))))) >0){
      tab.to.add3<-table(is.na(eval(parse_expr(paste0("dt_input$",contvars[i]))))
                         ,dt_input$college_edu, exclude=NULL)["TRUE",]
      tmp_cont_substr<-rbind(tmp_cont_substr, c(paste0(contvars[i],"_missing"),tab.to.add3)) 
    }
    tab.to.add4<-tapply(eval(parse_expr(paste0("dt_input$",contvars[i]))), 
                        dt_input$college_edu, median, na.rm=T)
    tmp_cont_substr<-rbind(tmp_cont_substr, c(paste0(contvars[i],"_median"),tab.to.add4)) 
    
    tab.to.add5<-tapply(eval(parse_expr(paste0("dt_input$",contvars[i]))), 
                        dt_input$college_edu, quantile, probs=0.25, na.rm=T)
    tmp_cont_substr<-rbind(tmp_cont_substr, c(paste0(contvars[i],"_q1"),tab.to.add5)) 
    
    tab.to.add6<-tapply(eval(parse_expr(paste0("dt_input$",contvars[i]))), 
                        dt_input$college_edu, quantile, probs=0.75, na.rm=T)
    tmp_cont_substr<-rbind(tmp_cont_substr, c(paste0(contvars[i],"_q3"),tab.to.add6)) 
  }
  tmp_cont_substr<-data.frame(tmp_cont_substr)
  return(tmp_cont_substr)
  }

T1results_cont_chinese <- table_education_cont_str_nonimp("Chinese") 
T1results_cont_japanese <- table_education_cont_str_nonimp("Japanese") 
T1results_cont_filipino <- table_education_cont_str_nonimp("Filipino") 
T1results_cont_substr <- 
  left_join(T1results_cont_chinese, T1results_cont_japanese, 
            by="Variablename") %>% left_join(., T1results_cont_filipino,
                                             by="Variablename")
T1results_cont_substr


#----Asian (each ethnicity) Table 1, imputed datset - categorical ----
#Getting values for the imputed dataset; there should be no missingness!
#----Each ethnicity----
table_education_str <- function(input){
  if(input == "Chinese"){ #ethnicity_rev == 2
    dt_input <- data_post_im[ethnicity_rev==2,]
  }else if(input == "Japanese"){ #ethnicity_rev == 3
    dt_input <- data_post_im[ethnicity_rev==3,]
  }else{ #ethnicity_rev == 5 (Filipino)
    dt_input <- data_post_im[ethnicity_rev==5,]
  }
  
  T1_education_str<-matrix(nrow=1, ncol=3) 
  T1_education_str[1,]<- c("Race/ethnicity total",
                               table(dt_input$college_edu, exclude = NULL)/20)
  
  for (i in 1:length(catvars)){
    tab.to.add<-table(eval(parse_expr(paste0("dt_input$",catvars[i]))),
                      dt_input$college_edu, exclude=NULL)/20
    labs<-paste(catvars[i],as.character(rownames(tab.to.add)))
    T1_education_str<-rbind(T1_education_str, c(paste(catvars[i]), rep(NA,2))) 
    T1_education_str<-rbind(T1_education_str,cbind(labs, tab.to.add))
  }
  #Notes: parse_expr() spits out the string as an expression. Must use eval() fxn
  #in order to use the expression (if eval() removed, will cause an error)
  
  colnames(T1_education_str)<-c("Variablename", paste0(input, "_less_than_college"), paste0(input, "_college_or_more")) 
  rownames(T1_education_str)<-NULL
  T1_education_str<-as.data.frame(T1_education_str)
  
  #Get %'s by race/ethnicity
  T1_prop_education_str<-matrix(nrow=1, ncol=3) 
  T1_prop_education_str[1,]<- c("Race/Ethnicity total",table(
    dt_input$college_edu)/nrow(dt_input))
  
  #Getting the denominators for the percentage calculations
  Margins<-as.numeric(table(dt_input$college_edu))
  
  for (i in 1:length(catvars)){
    tab.to.add<-t(t(table(eval(parse_expr(paste0("dt_input$",catvars[i])))
                          ,dt_input$college_edu, exclude=NULL))/Margins)
    labs<-paste(catvars[i],as.character(rownames(tab.to.add)))
    T1_prop_education_str<-rbind(T1_prop_education_str, c(paste(catvars[i]),rep(NA,2))) 
    T1_prop_education_str<-rbind(T1_prop_education_str,cbind(labs, tab.to.add))
  }
  
  colnames(T1_prop_education_str)<-c("Variablename", paste0(input, "_less_than_college_prop"), paste0(input, "_college_or_more_prop")) 
  rownames(T1_prop_education_str)<-NULL
  T1_prop_education_str<-as.data.frame(T1_prop_education_str)
  
  #merge n and % results
  T1_education_str<-left_join(T1_education_str, 
                                  T1_prop_education_str, by="Variablename")
  T1_education_str<-T1_education_str[,c("Variablename",
                                                paste0(input, "_less_than_college"), paste0(input, "_less_than_college_prop"),
                                                paste0(input, "_college_or_more"), paste0(input, "_college_or_more_prop"))]
  
  return(T1_education_str)
}

T1results_cat_impute_chinese <- table_education_str("Chinese")
T1results_cat_impute_japanese <- table_education_str("Japanese")
T1results_cat_impute_filipino <- table_education_str("Filipino")
T1results_cat_impute_substr <- 
  left_join(T1results_cat_impute_chinese, T1results_cat_impute_japanese, 
          by="Variablename") %>% left_join(., T1results_cat_impute_filipino,
                                           by="Variablename")

T1results_cat_impute_substr$Chinese_less_than_college_prop <- paste0("(",
                                                                round(as.numeric(T1results_cat_impute_substr$Chinese_less_than_college_prop)*100, 1),
                                                                ")") 
T1results_cat_impute_substr$Chinese_college_or_more_prop <- paste0("(",
                                                      round(as.numeric(T1results_cat_impute_substr$Chinese_college_or_more_prop)*100, 1),
                                                      ")") 
T1results_cat_impute_substr$Japanese_less_than_college_prop <- paste0("(",
                                                                round(as.numeric(T1results_cat_impute_substr$Japanese_less_than_college_prop)*100, 1),
                                                                ")") 
T1results_cat_impute_substr$Japanese_college_or_more_prop <- paste0("(",
                                                      round(as.numeric(T1results_cat_impute_substr$Japanese_college_or_more_prop)*100, 1),
                                                      ")") 
T1results_cat_impute_substr$Filipino_less_than_college_prop <- paste0("(",
                                                                round(as.numeric(T1results_cat_impute_substr$Filipino_less_than_college_prop)*100, 1),
                                                                ")") 
T1results_cat_impute_substr$Filipino_college_or_more_prop <- paste0("(",
                                                      round(as.numeric(T1results_cat_impute_substr$Filipino_college_or_more_prop)*100, 1),
                                                      ")") 
T1results_cat_impute_substr


#----Asian subgroups, imputed dataset - continuous----
#Pivoting long datasets to wide datasets for each of the variables

# College or more
cont_results_college_or_more <- 
  tibble("var" = 
           rep(c("main_dem_v1_fu_time", "survey_age", "income_pp", "ehr_ht_median"), 
               each = 5), 
         "stat" = rep(c("mean", "SD", "median", "q1", "q3"), 4), 
         "Asian" = 0, "2" = 0, "3" = 0, "5" = 0)

cont_results_college_or_more <- as.data.frame(cont_results_college_or_more)

for(var in c("main_dem_v1_fu_time", "survey_age", "income_pp", "ehr_ht_median")){
  # subset_test <- data_post_im[college_edu==1,] %>% 
  #   dplyr::select("subjid", "imp", "asian", "ethnicity_rev", "main_dem_v1_fu_time")
  
  subset <- data_post_im[college_edu==1,] %>% 
    dplyr::select("subjid", "imp", "asian", "ethnicity_rev", all_of(var))

  # AA_subset_test <- subset_test %>% filter(asian == 1) %>% 
  #   dplyr::select(c("subjid", "imp", "main_dem_v1_fu_time")) %>% 
  #   pivot_wider(id_cols = subjid, names_from = imp, values_from = main_dem_v1_fu_time)
    
  AA_subset <- subset %>% filter(asian == 1) %>% 
    dplyr::select(c("subjid", "imp", all_of(var))) %>% 
    pivot_wider(id_cols = subjid, names_from = imp, values_from = var)
  
  AA_data <- as.matrix(AA_subset[, 2:21])
  # AA_imp_mean = rowMeans(AA_data)
  # mean(AA_data, na.rm = T) # Sporadic NA's due to MI 
  
  # AA_data_test <- as.matrix(AA_subset_test[, 2:21])
  # AA_imp_mean_test = rowMeans(AA_data_test)
  
  # statistics over all matrix elements excluding NAs (04/22/2022)
  cont_results_college_or_more[which(cont_results_college_or_more$var == var & cont_results_college_or_more$stat == "mean"), 
               "Asian"] <- mean(AA_data, na.rm = T)
  cont_results_college_or_more[which(cont_results_college_or_more$var == var & cont_results_college_or_more$stat == "SD"), 
               "Asian"] <- sd(AA_data, na.rm = T)
  cont_results_college_or_more[which(cont_results_college_or_more$var == var & cont_results_college_or_more$stat == "median"), 
               "Asian"] <- median(AA_data, na.rm = T)
  cont_results_college_or_more[which(cont_results_college_or_more$var == var & cont_results_college_or_more$stat == "q1"), 
               "Asian"] <- quantile(AA_data, probs = 0.25, na.rm = T)
  cont_results_college_or_more[which(cont_results_college_or_more$var == var & cont_results_college_or_more$stat == "q3"), 
               "Asian"] <- quantile(AA_data, probs = 0.75, na.rm = T)
  
  
  for(group in names(table(subset$ethnicity_rev))){
    subgroup <- subset %>% filter(ethnicity_rev == group) %>% 
      dplyr::select(c("subjid", "imp", all_of(var))) %>% 
      pivot_wider(id_cols = subjid, names_from = imp, values_from = var)
    
    subgroup_data <- as.matrix(subgroup[, 2:21])
    
    # subgroup_imp_mean = rowMeans(subgroup_data) # Sporadic NA's due to MI 
    cont_results_college_or_more[which(cont_results_college_or_more$var == var & cont_results_college_or_more$stat == "mean"), 
                         group] <- mean(subgroup_data, na.rm = T)
    cont_results_college_or_more[which(cont_results_college_or_more$var == var & cont_results_college_or_more$stat == "SD"), 
                         group] <- sd(subgroup_data, na.rm = T)
    cont_results_college_or_more[which(cont_results_college_or_more$var == var & cont_results_college_or_more$stat == "median"), 
                         group] <- median(subgroup_data, na.rm = T)
    cont_results_college_or_more[which(cont_results_college_or_more$var == var & cont_results_college_or_more$stat == "q1"), 
                         group] <- quantile(subgroup_data, probs = 0.25, na.rm = T)
    cont_results_college_or_more[which(cont_results_college_or_more$var == var & cont_results_college_or_more$stat == "q3"), 
                         group] <- quantile(subgroup_data, probs = 0.75, na.rm = T)
  }
}
colnames(cont_results_college_or_more) <- c("var","stat","Asian_college_or_more","Chinese_college_or_more", 
                          "Japanese_college_or_more","Filipino_college_or_more")
cont_results_college_or_more$concat <- paste(cont_results_college_or_more$var, sep = "_", cont_results_college_or_more$stat)
cont_results_college_or_more

# Less than college
cont_results_less_than_college <- 
  tibble("var" = 
           rep(c("main_dem_v1_fu_time", "survey_age", "income_pp", "ehr_ht_median"), 
               each = 5), 
         "stat" = rep(c("mean", "SD", "median", "q1", "q3"), 4), 
         "Asian" = 0, "2" = 0, "3" = 0, "5" = 0)

cont_results_less_than_college <- as.data.frame(cont_results_less_than_college)

for(var in c("main_dem_v1_fu_time", "survey_age", "income_pp", "ehr_ht_median")){
  # subset_test <- data_post_im[college_edu==0,] %>% 
  #   dplyr::select("subjid", "imp", "asian", "ethnicity_rev", "main_dem_v1_fu_time")
  
  subset <- data_post_im[college_edu==0,] %>% 
    dplyr::select("subjid", "imp", "asian", "ethnicity_rev", all_of(var))
  
  # AA_subset_test <- subset_test %>% filter(asian == 1) %>% 
  #   dplyr::select(c("subjid", "imp", "main_dem_v1_fu_time")) %>% 
  #   pivot_wider(id_cols = subjid, names_from = imp, values_from = main_dem_v1_fu_time)
  
  AA_subset <- subset %>% filter(asian == 1) %>% 
    dplyr::select(c("subjid", "imp", all_of(var))) %>% 
    pivot_wider(id_cols = subjid, names_from = imp, values_from = var)
  
  AA_data <- as.matrix(AA_subset[, 2:21])
  # AA_imp_mean = rowMeans(AA_data)
  # mean(AA_data, na.rm = T) # Sporadic NA's due to MI 
  
  # AA_data_test <- as.matrix(AA_subset_test[, 2:21])
  # AA_imp_mean_test = rowMeans(AA_data_test)
  
  # statistics over all matrix elements excluding NAs (04/22/2022)
  cont_results_less_than_college[which(cont_results_less_than_college$var == var & cont_results_less_than_college$stat == "mean"), 
                       "Asian"] <- mean(AA_data, na.rm = T)
  cont_results_less_than_college[which(cont_results_less_than_college$var == var & cont_results_less_than_college$stat == "SD"), 
                       "Asian"] <- sd(AA_data, na.rm = T)
  cont_results_less_than_college[which(cont_results_less_than_college$var == var & cont_results_less_than_college$stat == "median"), 
                       "Asian"] <- median(AA_data, na.rm = T)
  cont_results_less_than_college[which(cont_results_less_than_college$var == var & cont_results_less_than_college$stat == "q1"), 
                       "Asian"] <- quantile(AA_data, probs = 0.25, na.rm = T)
  cont_results_less_than_college[which(cont_results_less_than_college$var == var & cont_results_less_than_college$stat == "q3"), 
                       "Asian"] <- quantile(AA_data, probs = 0.75, na.rm = T)
  
  
  for(group in names(table(subset$ethnicity_rev))){
    subgroup <- subset %>% filter(ethnicity_rev == group) %>% 
      dplyr::select(c("subjid", "imp", all_of(var))) %>% 
      pivot_wider(id_cols = subjid, names_from = imp, values_from = var)
    
    subgroup_data <- as.matrix(subgroup[, 2:21])
    
    # subgroup_imp_mean = rowMeans(subgroup_data) # Sporadic NA's due to MI 
    cont_results_less_than_college[which(cont_results_less_than_college$var == var & cont_results_less_than_college$stat == "mean"), 
                         group] <- mean(subgroup_data, na.rm = T)
    cont_results_less_than_college[which(cont_results_less_than_college$var == var & cont_results_less_than_college$stat == "SD"), 
                         group] <- sd(subgroup_data, na.rm = T)
    cont_results_less_than_college[which(cont_results_less_than_college$var == var & cont_results_less_than_college$stat == "median"), 
                         group] <- median(subgroup_data, na.rm = T)
    cont_results_less_than_college[which(cont_results_less_than_college$var == var & cont_results_less_than_college$stat == "q1"), 
                         group] <- quantile(subgroup_data, probs = 0.25, na.rm = T)
    cont_results_less_than_college[which(cont_results_less_than_college$var == var & cont_results_less_than_college$stat == "q3"), 
                         group] <- quantile(subgroup_data, probs = 0.75, na.rm = T)
  }
}
colnames(cont_results_less_than_college) <- c("var","stat","Asian_less_than_college","Chinese_less_than_college", 
                                    "Japanese_less_than_college","Filipino_less_than_college")
# cont_results_less_than_college <- cont_results_less_than_college %>% select(-c("var", "stat"))

cont_results <- merge(cont_results_college_or_more, cont_results_less_than_college, by = c("var", "stat")) 
cont_results <- cont_results %>% select(c("var", "stat", "concat",
                                          "Asian_less_than_college", "Asian_college_or_more",
                                          "Chinese_less_than_college", "Chinese_college_or_more",
                                          "Japanese_less_than_college", "Japanese_college_or_more",
                                          "Filipino_less_than_college", "Filipino_college_or_more"))
cont_results

#----Export results to a raw Excel----
t1_res_education_str <- 
  list(cat_education=T1results_cat, 
             cont_education=T1results_cont, 
             cat_education_imp=T1results_cat_impute,
             cat_substr_education = T1results_cat_substr,
             cont_substr_education = T1results_cont_substr,
             cat_education_imp_substr = T1results_cat_impute_substr,
             cont_education_imp_all = cont_results)

write.xlsx(t1_res_education_str,
           file = paste0(path_to_output, "gen_edu_Table1_education_str_raw_2022_12_07.xlsx"),
           overwrite = TRUE)
