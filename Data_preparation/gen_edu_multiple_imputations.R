# input
# gen_edu_data_for_mi_2022_04_18.sas7bdat
# R4.1.3

# objective of this file
# apply multiple imputations (m=20) and make imputed dataset

# models
# impute datasets (m=20)

#path to working directories
path_to_data <- "~/Library/CloudStorage/Box-Box/Asian_Americans_dementia_data/generation x education/Paper/2022-04-11-/"
path_to_output <- "~/Library/CloudStorage/Box-Box/Asian_Americans_dementia_data/generation x education/Paper/2022-04-11-/Imputed_data/"

#load packages
library(haven)
library(tidyverse)
library(mice)
library(data.table)
library(skimr)

#load gen_edu_data_for_mi_2022_04_18.sas7bdat -> data_en
data_pre_im <- read_sas(paste0(path_to_data,"gen_edu_data_for_mi_2022_04_18.sas7bdat"))
dim(data_pre_im) #14749*33

vars <- colnames(data_pre_im)
str(data_pre_im)
skim(data_pre_im)

#preliminary checks
summary(data_pre_im$main_dem_v1_fu_time) #0.005-18.049
summary(data_pre_im$main_dem_v1_end_age) #60.58-102.02
table(data_pre_im$main_dem_v1_end_age > data_pre_im$survey_age, exclude = NULL) #TRUE:14749
table(data_pre_im$main_dem_v1_end_type, exclude = NULL)

#check missingness 
summary(data_pre_im$survey_age) #59.99-89.99
summary(data_pre_im$income)
summary(data_pre_im$income_pp)

table(data_pre_im$female, exclude = NULL)
table(data_pre_im$sizeofhh, exclude = NULL)
table(data_pre_im$survey_language, exclude = NULL)
table(data_pre_im$ehr_ht_median, exclude = NULL)
table(data_pre_im$maritalstatus, exclude = NULL) #58-76
table(data_pre_im$smoking_status, exclude = NULL)
table(data_pre_im$generalhealth, exclude = NULL)
table(data_pre_im$sr_diabetes, exclude = NULL)
table(data_pre_im$sr_hyp, exclude = NULL)
table(data_pre_im$sr_stroke, exclude = NULL)
# table(data_pre_im$asian, exclude = NULL)
table(data_pre_im$education_rev, exclude = NULL)
table(data_pre_im$usaborn_rev, exclude = NULL)
table(data_pre_im$usabornfather_rev, exclude = NULL)
table(data_pre_im$usabornmother_rev, exclude = NULL)
# table(data_pre_im$asian, data_pre_im$ethnicity_rev, exclude = NULL)

#**Code adapted from EHL's KW multiple imputations script ("multiple_imputations.R")
#list of variables to impute/include in MICE (impute education (education_rev) & nativity)
impute.var.list<- c("survey_age", "female", "ethnicity_rev", "income", "income_pp","sizeofhh",
                    "education_rev", "smoking_status", "maritalstatus", "survey_language",
                    "usaborn_rev", "usabornfather_rev", "usabornmother_rev",
                    "generalhealth", "sr_stroke", "sr_diabetes", "sr_hyp","ehr_ht_median")

#---- check missing ----
#Assess missingness in vars we want to impute / use for analysis
missingsummary <- data.frame(varname = impute.var.list, pctmiss = NA)
row.names(missingsummary) <- impute.var.list
for (i in impute.var.list){
  missingsummary[i, "pctmiss"] <- 100*sum(is.na(data_pre_im[, i]))/nrow(data_pre_im)
  
  print(i)
  print(table(data_pre_im[, i], exclude = NULL))
}

missingordered <- missingsummary[order(missingsummary$pctmiss), ]
missingordered

ordered.var.list <- c(paste(missingordered$varname))

#--- data prep ----
## Run single imputation
#prep data by dropping vars we don't need, ordering by missingness
impute.data <- data_pre_im[, ordered.var.list]

#Set variable classes by type
str(impute.data)

#survey_age and income_pp continuous
#ehr_ht_median continuous (04/18/2022)

#binary vars -> "logreg" (logistic regression)
impute.data$female <- as.factor(impute.data$female)
impute.data$usaborn_rev <- as.factor(impute.data$usaborn_rev)
impute.data$usabornmother_rev <- as.factor(impute.data$usabornmother_rev)
impute.data$usabornfather_rev <- as.factor(impute.data$usabornfather_rev)

impute.data$sr_diabetes <- as.factor(impute.data$sr_diabetes) #(04/11/2022)
impute.data$sr_hyp <- as.factor(impute.data$sr_hyp) #(04/11/2022)
impute.data$sr_stroke <- as.factor(impute.data$sr_stroke) #(04/11/2022)

#categorical vars -> "polyreg" (nultinomial logit)
impute.data$survey_language <- factor(impute.data$survey_language, ordered = F)
impute.data$ethnicity_rev <- factor(impute.data$ethnicity_rev, ordered = F)
impute.data$maritalstatus <- factor(impute.data$maritalstatus, ordered = F)
impute.data$smoking_status <- factor(impute.data$smoking_status, ordered = F)

#ordinal vars -> "polr" (ordered logit)
impute.data$education_rev <- factor(impute.data$education_rev, ordered = T)
impute.data$income <- factor(impute.data$income, ordered = T)
impute.data$sizeofhh <- factor(impute.data$sizeofhh, ordered = T)
impute.data$generalhealth <- factor(impute.data$generalhealth, ordered = T) #(04/11/2022)

#recheck classes
str(impute.data)

#---- initiate imputations ----
#dry run (maxit = 0)
ini <- mice(impute.data, maxit = 0, 
            defaultMethod = c("pmm", "logreg", "polyreg", "polr"), seed = 12345) 
#create the mids object containing the default setting (method/predictorMatrix)

ini$method
meth <- ini$method 
meth

ini$predictorMatrix
pred <- ini$predictorMatrix

#change predictor matrix so income_pp doesn't predict income or sizeofhh
pred[c("income", "sizeofhh"), "income_pp"] <- 0 #specify the set of predictors to be used for each incomplete variable.
pred

#---- run imputations ----
# m=20, maxit = 10
t <- proc.time()
imp_fcs <- mice(impute.data, m = 20, maxit = 10, pred = pred, meth = meth, 
                defaultMethod = c("pmm", "logreg", "polyreg", "polr"), 
                seed = 12345)
proc.time() - t

#---- examine diagnostics ----
imp_fcs$loggedEvents #confirm no perfect-collinearity
plot(imp_fcs)
densityplot(imp_fcs, ~maritalstatus)
densityplot(imp_fcs, ~usabornmother_rev)
densityplot(imp_fcs, ~usabornfather_rev)
densityplot(imp_fcs, ~usaborn_rev)
densityplot(imp_fcs, ~sizeofhh)
densityplot(imp_fcs, ~income)
densityplot(imp_fcs, ~income_pp)
densityplot(imp_fcs, ~education_rev)
densityplot(imp_fcs, ~smoking_status)
densityplot(imp_fcs, ~generalhealth)

#stack imputed datasets
imp.temp <- list()
for (i in 1:20){
  imp.temp[[i]] <- complete(imp_fcs, action = i)
  
  imp.temp[[i]] <- cbind(data_pre_im$subjid, imp.temp[[i]][, c(impute.var.list)])
  imp.temp[[i]][, "imp"] <- i
}

gen_edu_fcs_stacked <- do.call(rbind, imp.temp)
colnames(gen_edu_fcs_stacked)
names(gen_edu_fcs_stacked)[names(gen_edu_fcs_stacked) == 
                                "data_pre_im$subjid"] <- "subjid"

#save stacked imputed dataset
save(gen_edu_fcs_stacked, 
     file = paste0(path_to_output, "gen_edu_imputed_stacked_2022_04_18.R"))

#--- Add other vars to dataset for analysis ---
load(paste0(path_to_output, "gen_edu_imputed_stacked_2022_04_18.R"))

other.vars <- data_pre_im[, !names(data_pre_im) %in% 
                            c("survey_age", "female", "ethnicity_rev", "income", "income_pp","sizeofhh",
                              "education_rev", "smoking_status", "maritalstatus", "survey_language",
                              "usaborn_rev", "usabornfather_rev", "usabornmother_rev",
                              "generalhealth", "sr_stroke", "sr_diabetes", "sr_hyp","ehr_ht_median")]


#Merge
gen_edu_analysis <- merge(x = gen_edu_fcs_stacked, y = other.vars,
                                 by = "subjid")

gen_edu_analysis <- as.data.table(gen_edu_analysis)

#---- clean covariates ----
#center age
gen_edu_analysis$age60 <- (gen_edu_analysis$survey_age - 60)

# #scale/centerincome_pp
# summary(gen_edu_analysis$income_pp)
# hist(gen_edu_analysis$income_pp)
# 
# gen_edu_analysis$incomepp_clean <- 
#   (gen_edu_analysis$income_pp - 
#      round(median(gen_edu_analysis$income_pp)))/10000

#set education as factor
table(gen_edu_analysis$education_rev, exclude = NULL) #no NA
table(gen_edu_analysis$college_edu, exclude = NULL) #795*20NAs
table(gen_edu_analysis$education_cat, exclude = NULL) #795*20NAs
gen_edu_analysis$education_rev <- 
  factor(gen_edu_analysis$education_rev, ordered = FALSE)

#dichotomous education variable (<college, college+)
gen_edu_analysis[,college_edu:=ifelse((education_rev==5|education_rev==6),1,0)]
table(gen_edu_analysis$college_edu, exclude = NULL) #no NAs

#categorical education variable (<12, 12-college, college or more)
gen_edu_analysis$education_cat <- "NA"
gen_edu_analysis[(education_rev==1|education_rev==2),education_cat:="Less than high school"]
gen_edu_analysis[(education_rev==3|education_rev==4),education_cat:="High school"]
gen_edu_analysis[(education_rev==5|education_rev==6),education_cat:="College or more"]
table(gen_edu_analysis$education_cat, exclude = NULL) #no NAs
gen_edu_analysis$education_cat <- relevel(factor(gen_edu_analysis$education_cat,ordered = F), ref = "High school")

# #create continuous education variable
# gen_edu_analysis[education_rev=="1",education_cont:=8]
# gen_edu_analysis[education_rev=="2",education_cont:=10]
# gen_edu_analysis[education_rev=="3",education_cont:=12]
# gen_edu_analysis[education_rev=="4",education_cont:=14]
# gen_edu_analysis[education_rev=="5",education_cont:=16]
# gen_edu_analysis[education_rev=="6",education_cont:=18]
# str(gen_edu_analysis$education_cont)
# table(gen_edu_analysis$education_cont, exclude = NULL)

#Sanity check
table(gen_edu_analysis$education_rev,gen_edu_analysis$college_edu,exclude = NULL)
table(gen_edu_analysis$education_rev,gen_edu_analysis$education_cat,exclude = NULL)

#---- Save stacked analytic dataset----
save(gen_edu_analysis, 
     file = paste0(path_to_output, "gen_edu_analysis_2022_04_18.R"))

