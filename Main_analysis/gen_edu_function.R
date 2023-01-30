# HR_fucntion (Referring Taylor's ADI code)
# Aalen_function

#---- Coxph custom HR function ---- (From Taylor's "HR_function.R")
# college*us_born
gen_edu_HR_calc_college_us_born <- function(input){
  # eval(parse_expr(paste0("cox_result_raw_", tolower("Asian"))))
  model <- eval(parse_expr(paste0("cox_result_raw_", tolower(input))))
  # vcov_matrix <- MIcombine(eval(parse_expr(paste0("cox_model_list_",tolower("Asian"))))) %>% vcov
  vcov_matrix <- MIcombine(eval(parse_expr(paste0("cox_model_list_",tolower(input))))) %>% vcov
  
  pt_est <- model["factor(college_edu)1", "results"] + 
    model["factor(college_edu)1:factor(usaborn_rev)1", "results"]
  
  sd <- 
    sqrt(vcov_matrix[paste0("factor(college_edu)1"),paste0("factor(college_edu)1")] + 
           vcov_matrix[paste0("factor(college_edu)1:factor(usaborn_rev)1"), 
                       paste0("factor(college_edu)1:factor(usaborn_rev)1")] + 
           2*
           vcov_matrix[paste0("factor(college_edu)1"), 
                       paste0("factor(college_edu)1:factor(usaborn_rev)1")])
  
  ll <- pt_est - 1.96*sd
  ul <- pt_est + 1.96*sd
  
  return(exp(c(pt_est, ll, ul)))
}

#---- Aalen custom additive function ---- (From Taylor's "HR_function.R")
# college*us_born
gen_edu_additive_calc_college_us_born <- function(input){
  # eval(parse_expr(paste0("aalen_result_raw_", tolower("Asian"))))
  model <- eval(parse_expr(paste0("aalen_result_raw_", tolower(input))))
  # vcov_matrix <- MIcombine(eval(parse_expr(paste0("beta_",tolower("Asian")))), MIextract(eval(parse_expr(paste0("aalen_model_list_",tolower("Asian")))),fun=vcov)) %>% vcov
  vcov_matrix <- MIcombine(eval(parse_expr(paste0("beta_",tolower(input)))),
                           MIextract(eval(parse_expr(paste0("aalen_model_list_",tolower(input)))),fun=vcov)) %>% vcov
  
  pt_est <- model["const(factor(college_edu))1", "results"] + 
    model["const(factor(college_edu))1:const(factor(usaborn_rev))1", "results"]
  
  sd <- 
    sqrt(vcov_matrix[paste0("const(factor(college_edu))1"),paste0("const(factor(college_edu))1")] + 
           vcov_matrix[paste0("const(factor(college_edu))1:const(factor(usaborn_rev))1"), 
                       paste0("const(factor(college_edu))1:const(factor(usaborn_rev))1")] + 
           2*
           vcov_matrix[paste0("const(factor(college_edu))1"), 
                       paste0("const(factor(college_edu))1:const(factor(usaborn_rev))1")])
  
  ll <- pt_est - 1.96*sd
  ul <- pt_est + 1.96*sd
  
  return(c(pt_est, ll, ul))
}
