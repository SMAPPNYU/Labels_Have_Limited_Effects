

#Load libraries:
library(dplyr)
library(xtable)
library(AER)
library(glmnet)
library(estimatr)
library(huxtable)
library(magrittr)
library(texreg)
library(ivpack)
library(ggplot2)
library(haven)
library(dplyr)
library(tidyverse)
library(ivdesc)


######### Run ITT Models and Covariate-Adjusted Complier Average Causal Effects (CACE) models using both the stronger and weaker compliance checks:

### Test Hypotheses 1, 2, and 3:
#(H1) We test whether in-feed source reliability labels shift downstream news and information consumption from unreliable sources known for publishing misleading or false content to more reliable sources
#(H2) We test whether it increases trust in mainstream media and reliable sources (H2), and  
#(H3) mitigate phenomena associated with democratic dysfunction (affective polarization and political cynicism). 

#Read in survey data with digital trace data:
Pulse_data <- read_csv('./Data/Clean_NewsGuard_Digital_Trace_Data.csv',
                       col_types = cols(
                         .default= col_character(),
                         BLM_info_Index_w2 = col_double(),
                         Covid_info_Index_w2 = col_double(),
                         BLM_Misinfo_Index_w2 = col_double(),
                         Covid_Misinfo_Index_w2 = col_double(),
                         Trust_Media_w2 = col_double(),
                         SMP4310_w2 = col_double(),
                         SMP4310 = col_double(),
                         Treated = col_double(),
                         Complied=col_double(),
                         Total_DL =  col_double(),
                         Total_Science_Misinfo  =  col_double(),
                         Social_Media_Use =  col_double(),
                         income_score =  col_double(),
                         abs_part =  col_double(),
                         aff_pol_w2 = col_double(),
                         aff_pol_w1 = col_double(),
                         mean_cons =  col_double(),
                         Average_domain_NewsG_Score_post= col_double(),
                         Average_domain_NewsG_Score= col_double(),
                         Prop_Unreliable_NewsG_Score_post= col_double(),
                         Prop_Unreliable_NewsG_Score= col_double(),
                         Prop_Reliable_NewsG_Score_post = col_double(),
                         Prop_Reliable_NewsG_Score = col_double(),
                         Count_Unreliable_NewsG_Score_post = col_double(),
                         Count_Unreliable_NewsG_Score = col_double(),
                         Count_Reliable_NewsG_Score_post = col_double(),
                         Count_Reliable_NewsG_Score = col_double(),
                         Prop_Unreliable_NewsG_Score_dv = col_double(),
                         Average_domain_NewsG_Score_dv = col_double(),
                         Prop_Reliable_NewsG_Score_dv = col_double(),
                         Count_Unreliable_NewsG_Score_dv = col_double(),
                         Count_Reliable_NewsG_Score_dv = col_double(),
                         Average_domain_NewsG_Score_post_dur= col_double(),
                         Average_domain_NewsG_Score_dur= col_double(),
                         Prop_Unreliable_NewsG_Score_post_dur= col_double(),
                         Prop_Unreliable_NewsG_Score_dur= col_double(),
                         Prop_Reliable_NewsG_Score_post_dur = col_double(),
                         Prop_Reliable_NewsG_Score_dur = col_double(),
                         Count_Unreliable_NewsG_Score_post_dur = col_double(),
                         Count_Unreliable_NewsG_Score_dur = col_double(),
                         Count_Reliable_NewsG_Score_post_dur = col_double(),
                         Count_Reliable_NewsG_Score_dur = col_double(),
                         Prop_Unreliable_NewsG_Score_dv_dur = col_double(),
                         Average_domain_NewsG_Score_dv_dur = col_double(),
                         Prop_Reliable_NewsG_Score_dv_dur = col_double(),
                         Count_Unreliable_NewsG_Score_dv_dur = col_double(),
                         Count_Reliable_NewsG_Score_dv_dur = col_double(),
                         Prop_Unreliable_NewsG_Score_dv_SM_ref = col_double(),
                         Average_domain_NewsG_Score_dv_SM_ref = col_double(),
                         Prop_Reliable_NewsG_Score_dv_SM_ref = col_double(),
                         Count_Unreliable_NewsG_Score_dv_SM_ref = col_double(),
                         Count_Reliable_NewsG_Score_dv_SM_ref = col_double(),
                         Average_domain_NewsG_Score_post_SM_ref= col_double(),
                         Average_domain_NewsG_Score_SM_ref= col_double(),
                         Prop_Unreliable_NewsG_Score_post_SM_ref= col_double(),
                         Prop_Unreliable_NewsG_Score_SM_ref= col_double(),
                         Prop_Reliable_NewsG_Score_post_SM_ref = col_double(),
                         Prop_Reliable_NewsG_Score_SM_ref = col_double(),
                         Count_Unreliable_NewsG_Score_post_SM_ref = col_double(),
                         Count_Unreliable_NewsG_Score_SM_ref = col_double(),
                         Count_Reliable_NewsG_Score_post_SM_ref = col_double(),
                         Count_Reliable_NewsG_Score_SM_ref = col_double(),
                         Prop_Unreliable_NewsG_Score_dv_All_ref = col_double(),
                         Average_domain_NewsG_Score_dv_All_ref = col_double(),
                         Prop_Reliable_NewsG_Score_dv_All_ref = col_double(),
                         Count_Unreliable_NewsG_Score_dv_All_ref = col_double(),
                         Count_Reliable_NewsG_Score_dv_All_ref = col_double(),
                         Average_domain_NewsG_Score_post_All_ref= col_double(),
                         Average_domain_NewsG_Score_All_ref= col_double(),
                         Prop_Unreliable_NewsG_Score_post_All_ref= col_double(),
                         Prop_Unreliable_NewsG_Score_All_ref= col_double(),
                         Prop_Reliable_NewsG_Score_post_All_ref = col_double(),
                         Prop_Reliable_NewsG_Score_All_ref = col_double(),
                         Count_Unreliable_NewsG_Score_post_All_ref = col_double(),
                         Count_Unreliable_NewsG_Score_All_ref = col_double(),
                         Count_Reliable_NewsG_Score_post_All_ref = col_double(),
                         Count_Reliable_NewsG_Score_All_ref = col_double(),
                         gender_dummy_fem=col_double(),
                         educ_score=col_double(),
                         Age=col_double(),
                         Age_Sq=col_double(),
                         SMP4326_w2=col_double(),
                         SMP4326=col_double(),
                         Trust_inst_w2=col_double(),
                         Trust_inst_w1=col_double(),
                         Pol_cyn_2=col_double(),
                         Pol_cyn_1=col_double(),
                         Fox_Trust_2=col_double(),
                         Fox_Trust_1=col_double(),
                         CNN_Trust_2=col_double(),
                         CNN_Trust_1=col_double(),
                         ABC_Trust_2=col_double(),
                         ABC_Trust_1=col_double(),
                         NBC_Trust_2=col_double(),
                         NBC_Trust_1=col_double(),
                         CBS_Trust_2=col_double(),
                         CBS_Trust_1=col_double(),
                         party_score=col_double(),
                         race_white=col_double(),
                         ideo_score=col_double(),
                         Trust_Media_w1=col_double(),
                         trust_news=col_double(),
                         trust_news_sm=col_double(),
                         cons_news_n=col_double(),
                         cons_cable=col_double(),
                         cons_print=col_double(),
                         cons_public=col_double(), 
                         cons_talk=col_double(),
                         cons_desk=col_double(),
                         cons_mobile=col_double(),
                         Safari_dummy=col_double(),
                         log_news=col_double(),
                         Safari_dummy=col_double(),
                         IE_dummy=col_double(),
                         Chrome_dummy=col_double(),
                         Firefox_dummy=col_double(),
                         Social_Media_Use=col_double()))



#Read in survey data without digital trace data:

data_frame_1 <- read_csv('./Data/Clean_NewsGuard_Survey_Study.csv',
                         col_types = cols(
                           .default= col_character(),
                           Treated = col_double(),
                           Pulse_Dummy=col_double(),
                           Complied=col_double(),
                           Total_DL =  col_double(),
                           Inverse_DL = col_double(),
                           income_score =  col_double(),
                           Total_Science_Misinfo  =  col_double(),
                           Social_Media_Use =  col_double(),
                           mean_cons =  col_double(),
                           SMP4310=col_double(),
                           SMP4310_w2=col_double(),
                           SMP4326=col_double(),
                           SMP4326_w2=col_double(),
                           Trust_inst_w2=col_double(),
                           Trust_inst_w1=col_double(),
                           BLM_Misinfo_Index_w2=col_double(),
                           Covid_Misinfo_Index_w2=col_double(),
                           BLM_info_Index_w2=col_double(),
                           Covid_info_Index_w2=col_double(),
                           Pol_cyn_2=col_double(),
                           Pol_cyn_1=col_double(),
                           Fox_Trust_2=col_double(),
                           Fox_Trust_1=col_double(),
                           CNN_Trust_2=col_double(),
                           CNN_Trust_1=col_double(),
                           ABC_Trust_2=col_double(),
                           ABC_Trust_1=col_double(),
                           NBC_Trust_2=col_double(),
                           NBC_Trust_1=col_double(),
                           CBS_Trust_2=col_double(),
                           CBS_Trust_1=col_double(),
                           aff_pol_w2=col_double(),
                           aff_pol_w1=col_double(),
                           Trust_Media_w2=col_double(),
                           Treated=col_double(),
                           Trust_Media_w1=col_double(),
                           gender_dummy_fem=col_double(),
                           educ_score=col_double(),
                           Age=col_double(),
                           Age_Sq=col_double(),
                           party_score=col_double(),
                           race_white=col_double(),
                           ideo_score=col_double(),
                           Trust_Media_w1=col_double(),
                           trust_news=col_double(),
                           trust_news_sm=col_double(),
                           cons_news_n=col_double(),
                           cons_cable=col_double(),
                           cons_print=col_double(),
                           cons_public=col_double(), 
                           cons_talk=col_double(),
                           cons_desk=col_double(),
                           cons_mobile=col_double(),
                           Safari_dummy=col_double(),
                           IE_dummy=col_double(),
                           Chrome_dummy=col_double(),
                           Firefox_dummy=col_double(),
                           Social_Media_Use=col_double()
                         ))

#Set order of dataset:
data_frame_1 <- data_frame_1[order(data_frame_1$visa1),]



#Clean Data:

Clean <- function(data_for_analysis) {
  #Replace Infinite values in data with NA
  data_for_analysis <- do.call(data.frame,                      
                               lapply(data_for_analysis,
                                      function(x) replace(x, is.infinite(x), NA)))
  #Remove NA values:
  data_for_analysis <- na.omit(data_for_analysis)
  
  return(data_for_analysis)
}


#Create a function using glmnet lasso that chooses the variables to use:
Lasso <- function(data_for_analysis) {
  set.seed(938)
  lasso_select <- cv.glmnet(x=as.matrix(data_for_analysis[,-1]),
                            y=as.vector(data_for_analysis[,1]),
                            alpha=1)
  
  coef.out <- coef(lasso_select, exact = TRUE)
  indices <- which(coef.out != 0)
  names_of_columns <- c(rownames(coef.out)[indices],colnames(data_for_analysis)[1])
  names_of_columns_3 <- names_of_columns[!names_of_columns %in% "(Intercept)"]
  return(names_of_columns_3)
}




#Pulse_data <- Pulse_data %>% filter(Prop_Unreliable_NewsG_Score > quantile(Pulse_data$Prop_Unreliable_NewsG_Score)[3])



################## Behavioral Measures (Covariate-Adjusted Models) ##############################################

#List of possible variables for inclusion:
list_possible_covariates <- c("gender_dummy_fem",
                              "educ_score",
                              "Age",
                              "Age_Sq",
                              "party_score",
                              "race_white",
                              "ideo_score",
                              "Trust_Media_w1",
                              "trust_news",
                              "trust_news_sm",
                              "cons_news_n",
                              "cons_cable",
                              "cons_print",
                              "cons_public", 
                              "cons_talk",
                              "cons_desk",
                              "cons_mobile",
                              "log_news",
                              "Safari_dummy",
                              "IE_dummy",
                              "Chrome_dummy",
                              "Firefox_dummy",
                              "Social_Media_Use")

#Create Sets of variables to use in models:
list_variables_to_run_1 = list(c('Prop_Unreliable_NewsG_Score_dv_dur','Prop_Unreliable_NewsG_Score_dur'),
                               c('Prop_Reliable_NewsG_Score_dv_dur','Prop_Reliable_NewsG_Score_dur'),
                               c('Count_Unreliable_NewsG_Score_dv_dur','Count_Unreliable_NewsG_Score_dur'),
                               c('Count_Reliable_NewsG_Score_dv_dur','Count_Reliable_NewsG_Score_dur'),
                               c('Average_domain_NewsG_Score_dv_dur','Average_domain_NewsG_Score_dur'),
                               c('Prop_Unreliable_NewsG_Score_post_dur','Prop_Unreliable_NewsG_Score_dur'),
                               c('Prop_Reliable_NewsG_Score_post_dur','Prop_Reliable_NewsG_Score_dur'),
                               c('Count_Unreliable_NewsG_Score_post_dur','Count_Unreliable_NewsG_Score_dur'),
                               c('Count_Reliable_NewsG_Score_post_dur','Count_Reliable_NewsG_Score_dur'),
                               c('Average_domain_NewsG_Score_post_dur','Average_domain_NewsG_Score_dur'))

#Create list of Titles:
Titles <- c('Testing the Effect of the Intervention on Proportion of News Diet That is Unreliable with Covariate-Adjusted Models (HC2 Robust standard errors) (Before July 1st)  -- Duration Weighted',
            'Testing the Effect of the Intervention on Proportion of News Diet That is Reliable with Covariate-Adjusted Models (HC2 Robust standard errors) (Before July 1st)  -- Duration Weighted',
            'Testing the Effect of the Intervention on Count of Unreliable News Consumed with Covariate-Adjusted Models (HC2 Robust standard errors) (Before July 1st)  -- Duration Weighted',
            'Testing the Effect of the Intervention on Count of Reliable News Consumed with Covariate-Adjusted Models (HC2 Robust standard errors) (Before July 1st)  -- Duration Weighted',
            'Testing the Effect of the Intervention on Reliability Score of News Diet with Covariate-Adjusted Models (HC2 Robust standard errors) (Before July 1st)  -- Duration Weighted',
            'Testing the Effect of the Intervention on Proportion of News Diet That is Unreliable with Covariate-Adjusted Models (HC2 Robust standard errors) (After July 1st)  -- Duration Weighted',
            'Testing the Effect of the Intervention on Proportion of News Diet That is Reliable with Covariate-Adjusted Models (HC2 Robust standard errors) (After July 1st)  -- Duration Weighted',
            'Testing the Effect of the Intervention on Count of Unreliable News Consumed with Covariate-Adjusted Models (HC2 Robust standard errors) (After July 1st)  -- Duration Weighted',
            'Testing the Effect of the Intervention on Count of Reliable News Consumed with Covariate-Adjusted Models (HC2 Robust standard errors) (After July 1st)  -- Duration Weighted',
            'Testing the Effect of the Intervention on Reliability Score of News Diet with Covariate-Adjusted Models (HC2 Robust standard errors) (After July 1st)  -- Duration Weighted')



#Create list of filenames:
Tables_title <-  c('./Tables/Table_103.txt',
                   './Tables/Table_104.txt',
                   './Tables/Table_105.txt',
                   './Tables/Table_106.txt',
                   './Tables/Table_107.txt',
                   './Tables/Table_108.txt',
                   './Tables/Table_109.txt',
                   './Tables/Table_110.txt',
                   './Tables/Table_111.txt',
                   './Tables/Table_112.txt')

#Create crosswalk for variables and display names:
top_attribute_names <- c("gender_dummy_fem",
                         "educ_score",
                         'Treated',
                         "Age",
                         "Age_Sq",
                         "party_score",
                         "race_white",
                         "ideo_score",
                         "Trust_Media_w1",
                         "trust_news",
                         "trust_news_sm",
                         "cons_news_n",
                         "cons_cable",
                         "cons_print",
                         "cons_public", 
                         "cons_talk",
                         "cons_desk",
                         "cons_mobile",
                         "Safari_dummy",
                         "log_news",
                         'Prop_Unreliable_NewsG_Score',
                         'Prop_Reliable_NewsG_Score',
                         'Count_Unreliable_NewsG_Score',
                         'Count_Reliable_NewsG_Score',
                         'Average_domain_NewsG_Score',
                         'compliance_check_1',
                         'Complied',
                         "IE_dummy",
                         "Chrome_dummy",
                         "Firefox_dummy",
                         "Social_Media_Use")

top_attributes_html <- c('Gender',
                         'Education',
                         'Treatment',
                         'Age',
                         'Age-Squared',
                         "Party ID",
                         "Race/Ethnicity",
                         "Ideology ",
                         "Trust in Media",
                         "Trust of news in newspapers",
                         "Trust of news on social media ",
                         "News consumption (network news)",
                         "News consumption (cable news)",
                         "News consumption (print news) ",
                         "News consumption (public radio)", 
                         "News consumption (talk radio)",
                         "News consumption (news on desktop)",
                         "News consumption (news on mobile)",
                         "Web Browser (Safari)",
                         "Log of news viewed",
                         'Pre-Treatment Value',
                         'Pre-Treatment Value',
                         'Pre-Treatment Value',
                         'Pre-Treatment Value',
                         'Pre-Treatment Value',
                         'Treatment',
                         'Treatment',
                         "Internet Explorer Browser",
                         "Chrome Browser",
                         "Firefox Browser",
                         "Social Media use")

names(top_attributes_html) <- top_attribute_names


#Run For loop to produce Tables 3-12:


i=1
for(i in 1:length(list_variables_to_run_1)){
  list_possible_covariates_for_use <- c(list_variables_to_run_1[[i]],list_possible_covariates)
  
  data_for_analysis <- Pulse_data %>% ungroup() %>% select(`list_possible_covariates_for_use`)
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  #Use glmnet lasso to choose covariates to be a part of model:
  names_of_columns <- Lasso(data_for_analysis)
  
  #names_of_columns
  
  names_of_columns <- c('Treated',names_of_columns)
  
  
  names_use <- names(Pulse_data)[(names(Pulse_data) %in% names_of_columns)]
  
  data_for_analysis <- Pulse_data[, names_use]
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  f <- paste0(list_variables_to_run_1[[i]][1], " ~ .")
  
  ITT_Model <- lm_robust(as.formula(f), data = data_for_analysis)
  
  
  names_of_columns_2 <- c(names_of_columns,'compliance_check_1')
  names_use <- names(Pulse_data)[(names(Pulse_data) %in% names_of_columns_2)]
  data_for_analysis <- Pulse_data[, names_use]
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  f <- paste0(list_variables_to_run_1[[i]][1], '~ . - Treated | . - compliance_check_1')
  
  #CACE Model 1
  CACE_Model_1 <- iv_robust(as.formula(f), data = data_for_analysis)
  
  CACE_Model_1$term[which(CACE_Model_1$term == "compliance_check_11")] <- 'Treated'
  
  # #CACE Model 2 - Passed first and second wave compliance check
  names_of_columns_3 <- c(names_of_columns,'Complied')
  names_use <- names(Pulse_data)[(names(Pulse_data) %in% names_of_columns_3)]
  data_for_analysis <- Pulse_data[, names_use]
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  f <- paste0(list_variables_to_run_1[[i]][1], '~  . - Treated | . - Complied')
  CACE_Model_2 <- iv_robust(as.formula(f), data = data_for_analysis)
  
  CACE_Model_2$term[which(CACE_Model_2$term == "Complied")] <- 'Treated'
  
  #names of the variables:
  Variable_Names <- names(ITT_Model$coefficients)[-1]
  Variable_Names <- str_replace_all(Variable_Names, top_attributes_html)
  
  #Write Table
  texreg(list(ITT_Model,CACE_Model_1,CACE_Model_2),
         include.ci = FALSE,
         digits=4,
         omit.coef = '(Intercept)',
         caption= Titles[i],
         label = "table",
         include.rmse = FALSE,
         custom.coef.names = Variable_Names,
         custom.model.names= c("Intent-To-Treat (ITT)", "CACE (Model 1)","CACE (Model 2)"),
         file=Tables_title[i],
         float.pos = "!htbp",
         caption.above = TRUE)
}



################## Behavioral Measures (Covariate-Adjusted Models) - Referral ##############################################

#List of possible variables for inclusion:
list_possible_covariates <- c("gender_dummy_fem",
                              "educ_score",
                              "Age",
                              "Age_Sq",
                              "party_score",
                              "race_white",
                              "ideo_score",
                              "Trust_Media_w1",
                              "trust_news",
                              "trust_news_sm",
                              "cons_news_n",
                              "cons_cable",
                              "cons_print",
                              "cons_public", 
                              "cons_talk",
                              "cons_desk",
                              "cons_mobile",
                              "log_news",
                              "Safari_dummy",
                              "IE_dummy",
                              "Chrome_dummy",
                              "Firefox_dummy",
                              "Social_Media_Use")

#Create Sets of variables to use in models:
list_variables_to_run_1 = list(c('Prop_Unreliable_NewsG_Score_dv_All_ref','Prop_Unreliable_NewsG_Score_All_ref'),
                               c('Prop_Reliable_NewsG_Score_dv_All_ref','Prop_Reliable_NewsG_Score_All_ref'),
                               c('Count_Unreliable_NewsG_Score_dv_All_ref','Count_Unreliable_NewsG_Score_All_ref'),
                               c('Count_Reliable_NewsG_Score_dv_All_ref','Count_Reliable_NewsG_Score_All_ref'),
                               c('Average_domain_NewsG_Score_dv_All_ref','Average_domain_NewsG_Score_All_ref'),
                               c('Prop_Unreliable_NewsG_Score_post_All_ref','Prop_Unreliable_NewsG_Score_All_ref'),
                               c('Prop_Reliable_NewsG_Score_post_All_ref','Prop_Reliable_NewsG_Score_All_ref'),
                               c('Count_Unreliable_NewsG_Score_post_All_ref','Count_Unreliable_NewsG_Score_All_ref'),
                               c('Count_Reliable_NewsG_Score_post_All_ref','Count_Reliable_NewsG_Score_All_ref'),
                               c('Average_domain_NewsG_Score_post_All_ref','Average_domain_NewsG_Score_All_ref'))




#Create list of Titles:
Titles <- c('Testing the Effect of the Intervention on Proportion of News Diet of Referrals From Search Engines and Social Media (Google, Twitter, and Facebook) That is Unreliable with Covariate-Adjusted Models (HC2 Robust standard errors) (Before July 1st)',
            'Testing the Effect of the Intervention on Proportion of News Diet of Referrals From Search Engines and Social Media (Google, Twitter, and Facebook) That is Reliable with Covariate-Adjusted Models (HC2 Robust standard errors) (Before July 1st)',
            'Testing the Effect of the Intervention on Count of Unreliable News Referred From Search Engines and Social Media (Google, Twitter, and Facebook) with Covariate-Adjusted Models (HC2 Robust standard errors) (Before July 1st)',
            'Testing the Effect of the Intervention on Count of Reliable News Referred From Search Engines and Social Media (Google, Twitter, and Facebook) with Covariate-Adjusted Models (HC2 Robust standard errors) (Before July 1st)',
            'Testing the Effect of the Intervention on Reliability Score of News Referred From Search Engines and Social Media (Google, Twitter, and Facebook) with Covariate-Adjusted Models (HC2 Robust standard errors) (Before July 1st)',
            'Testing the Effect of the Intervention on Proportion of News Diet of Referrals From Search Engines and Social Media (Google, Twitter, and Facebook) That is Unreliable with Covariate-Adjusted Models (HC2 Robust standard errors) (After July 1st)',
            'Testing the Effect of the Intervention on Proportion News Diet of Referrals From Search Engines and Social Media (Google, Twitter, and Facebook) That is Reliable with Covariate-Adjusted Models (HC2 Robust standard errors) (After July 1st)',
            'Testing the Effect of the Intervention on Count of Unreliable News Referred From Search Engines and Social Media (Google, Twitter, and Facebook) with Covariate-Adjusted Models (HC2 Robust standard errors) (After July 1st)',
            'Testing the Effect of the Intervention on Count of Reliable News Referred From Search Engines and Social Media (Google, Twitter, and Facebook) with Covariate-Adjusted Models (HC2 Robust standard errors) (After July 1st)',
            'Testing the Effect of the Intervention on Reliability Score of News Referred From Search Engines and Social Media (Google, Twitter, and Facebook) with Covariate-Adjusted Models (HC2 Robust standard errors) (After July 1st)')



#Create list of filenames:
Tables_title <-  c('./Tables/Table_123.txt',
                   './Tables/Table_124.txt',
                   './Tables/Table_125.txt',
                   './Tables/Table_126.txt',
                   './Tables/Table_127.txt',
                   './Tables/Table_128.txt',
                   './Tables/Table_129.txt',
                   './Tables/Table_130.txt',
                   './Tables/Table_131.txt',
                   './Tables/Table_132.txt')

#Create crosswalk for variables and display names:
top_attribute_names <- c("gender_dummy_fem",
                         "educ_score",
                         'Treated',
                         "Age",
                         "Age_Sq",
                         "party_score",
                         "race_white",
                         "ideo_score",
                         "Trust_Media_w1",
                         "trust_news",
                         "trust_news_sm",
                         "cons_news_n",
                         "cons_cable",
                         "cons_print",
                         "cons_public", 
                         "cons_talk",
                         "cons_desk",
                         "cons_mobile",
                         "Safari_dummy",
                         "log_news",
                         'Prop_Unreliable_NewsG_Score',
                         'Prop_Reliable_NewsG_Score',
                         'Count_Unreliable_NewsG_Score',
                         'Count_Reliable_NewsG_Score',
                         'Average_domain_NewsG_Score',
                         'compliance_check_1',
                         'Complied',
                         "IE_dummy",
                         "Chrome_dummy",
                         "Firefox_dummy",
                         "Social_Media_Use")

top_attributes_html <- c('Gender',
                         'Education',
                         'Treatment',
                         'Age',
                         'Age-Squared',
                         "Party ID",
                         "Race/Ethnicity",
                         "Ideology ",
                         "Trust in Media",
                         "Trust of news in newspapers",
                         "Trust of news on social media ",
                         "News consumption (network news)",
                         "News consumption (cable news)",
                         "News consumption (print news) ",
                         "News consumption (public radio)", 
                         "News consumption (talk radio)",
                         "News consumption (news on desktop)",
                         "News consumption (news on mobile)",
                         "Web Browser (Safari)",
                         "Log of news viewed",
                         'Pre-Treatment Value',
                         'Pre-Treatment Value',
                         'Pre-Treatment Value',
                         'Pre-Treatment Value',
                         'Pre-Treatment Value',
                         'Treatment',
                         'Treatment',
                         "Internet Explorer Browser",
                         "Chrome Browser",
                         "Firefox Browser",
                         "Social Media use")

names(top_attributes_html) <- top_attribute_names


#Run For loop to produce Tables 3-12:


i=1
for(i in 1:length(list_variables_to_run_1)){
  list_possible_covariates_for_use <- c(list_variables_to_run_1[[i]],list_possible_covariates)
  
  data_for_analysis <- Pulse_data %>% ungroup() %>% select(`list_possible_covariates_for_use`)
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  #Use glmnet lasso to choose covariates to be a part of model:
  names_of_columns <- Lasso(data_for_analysis)
  
  #names_of_columns
  
  names_of_columns <- c('Treated',names_of_columns)
  
  
  names_use <- names(Pulse_data)[(names(Pulse_data) %in% names_of_columns)]
  
  data_for_analysis <- Pulse_data[, names_use]
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  f <- paste0(list_variables_to_run_1[[i]][1], " ~ .")
  
  ITT_Model <- lm_robust(as.formula(f), data = data_for_analysis)
  
  
  names_of_columns_2 <- c(names_of_columns,'compliance_check_1')
  names_use <- names(Pulse_data)[(names(Pulse_data) %in% names_of_columns_2)]
  data_for_analysis <- Pulse_data[, names_use]
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  f <- paste0(list_variables_to_run_1[[i]][1], '~ . - Treated | . - compliance_check_1')
  
  #CACE Model 1
  CACE_Model_1 <- iv_robust(as.formula(f), data = data_for_analysis)
  
  CACE_Model_1$term[which(CACE_Model_1$term == "compliance_check_11")] <- 'Treated'
  
  # #CACE Model 2 - Passed first and second wave compliance check
  names_of_columns_3 <- c(names_of_columns,'Complied')
  names_use <- names(Pulse_data)[(names(Pulse_data) %in% names_of_columns_3)]
  data_for_analysis <- Pulse_data[, names_use]
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  f <- paste0(list_variables_to_run_1[[i]][1], '~  . - Treated | . - Complied')
  CACE_Model_2 <- iv_robust(as.formula(f), data = data_for_analysis)
  
  CACE_Model_2$term[which(CACE_Model_2$term == "Complied")] <- 'Treated'
  
  #names of the variables:
  Variable_Names <- names(ITT_Model$coefficients)[-1]
  Variable_Names <- str_replace_all(Variable_Names, top_attributes_html)
  
  #Write Table
  texreg(list(ITT_Model,CACE_Model_1,CACE_Model_2),
         include.ci = FALSE,
         digits=4,
         omit.coef = '(Intercept)',
         caption= Titles[i],
         label = "table",
         include.rmse = FALSE,
         custom.coef.names = Variable_Names,
         custom.model.names= c("Intent-To-Treat (ITT)", "CACE (Model 1)","CACE (Model 2)"),
         file=Tables_title[i],
         float.pos = "!htbp",
         caption.above = TRUE)
}

################## Behavioral Measures (Covariate-Adjusted Models) - Referral ##############################################

#List of possible variables for inclusion:
list_possible_covariates <- c("gender_dummy_fem",
                              "educ_score",
                              "Age",
                              "Age_Sq",
                              "party_score",
                              "race_white",
                              "ideo_score",
                              "Trust_Media_w1",
                              "trust_news",
                              "trust_news_sm",
                              "cons_news_n",
                              "cons_cable",
                              "cons_print",
                              "cons_public", 
                              "cons_talk",
                              "cons_desk",
                              "cons_mobile",
                              "log_news",
                              "Safari_dummy",
                              "IE_dummy",
                              "Chrome_dummy",
                              "Firefox_dummy",
                              "Social_Media_Use")

#Create Sets of variables to use in models:
list_variables_to_run_1 = list(c('Prop_Unreliable_NewsG_Score_dv_SM_ref','Prop_Unreliable_NewsG_Score_SM_ref'),
                               c('Prop_Reliable_NewsG_Score_dv_SM_ref','Prop_Reliable_NewsG_Score_SM_ref'),
                               c('Count_Unreliable_NewsG_Score_dv_SM_ref','Count_Unreliable_NewsG_Score_SM_ref'),
                               c('Count_Reliable_NewsG_Score_dv_SM_ref','Count_Reliable_NewsG_Score_SM_ref'),
                               c('Average_domain_NewsG_Score_dv_SM_ref','Average_domain_NewsG_Score_SM_ref'),
                               c('Prop_Unreliable_NewsG_Score_post_SM_ref','Prop_Unreliable_NewsG_Score_SM_ref'),
                               c('Prop_Reliable_NewsG_Score_post_SM_ref','Prop_Reliable_NewsG_Score_SM_ref'),
                               c('Count_Unreliable_NewsG_Score_post_SM_ref','Count_Unreliable_NewsG_Score_SM_ref'),
                               c('Count_Reliable_NewsG_Score_post_SM_ref','Count_Reliable_NewsG_Score_SM_ref'),
                               c('Average_domain_NewsG_Score_post_SM_ref','Average_domain_NewsG_Score_SM_ref'))

#Create list of Titles:
Titles <- c('Testing the Effect of the Intervention on Proportion of News Diet of Referrals From Social Media (Twitter and Facebook) That is Unreliable with Covariate-Adjusted Models (HC2 Robust standard errors) (Before July 1st)',
            'Testing the Effect of the Intervention on Proportion of News Diet of Referrals From Social Media (Twitter and Facebook) That is Reliable with Covariate-Adjusted Models (HC2 Robust standard errors) (Before July 1st)',
            'Testing the Effect of the Intervention on Count of Unreliable News Referred From Social Media (Twitter and Facebook) with Covariate-Adjusted Models (HC2 Robust standard errors) (Before July 1st)',
            'Testing the Effect of the Intervention on Count of Reliable News Referred From Social Media (Twitter and Facebook) with Covariate-Adjusted Models (HC2 Robust standard errors) (Before July 1st)',
            'Testing the Effect of the Intervention on Reliability Score of News Referred From Social Media (Twitter and Facebook) with Covariate-Adjusted Models (HC2 Robust standard errors) (Before July 1st)',
            'Testing the Effect of the Intervention on Proportion of News Diet of Referrals From Social Media (Twitter and Facebook) That is Unreliable with Covariate-Adjusted Models (HC2 Robust standard errors) (After July 1st)',
            'Testing the Effect of the Intervention on Proportion News Diet of Referrals From Social Media (Twitter and Facebook) That is Reliable with Covariate-Adjusted Models (HC2 Robust standard errors) (After July 1st)',
            'Testing the Effect of the Intervention on Count of Unreliable News Referred From Social Media (Twitter and Facebook) with Covariate-Adjusted Models (HC2 Robust standard errors) (After July 1st)',
            'Testing the Effect of the Intervention on Count of Reliable News Referred From Social Media (Twitter and Facebook) with Covariate-Adjusted Models (HC2 Robust standard errors) (After July 1st)',
            'Testing the Effect of the Intervention on Reliability Score of News Referred From Social Media (Twitter and Facebook) with Covariate-Adjusted Models (HC2 Robust standard errors) (After July 1st)')



#Create list of filenames:
Tables_title <-  c('./Tables/Table_113.txt',
                   './Tables/Table_114.txt',
                   './Tables/Table_115.txt',
                   './Tables/Table_116.txt',
                   './Tables/Table_117.txt',
                   './Tables/Table_118.txt',
                   './Tables/Table_119.txt',
                   './Tables/Table_120.txt',
                   './Tables/Table_121.txt',
                   './Tables/Table_122.txt')

#Create crosswalk for variables and display names:
top_attribute_names <- c("gender_dummy_fem",
                         "educ_score",
                         'Treated',
                         "Age",
                         "Age_Sq",
                         "party_score",
                         "race_white",
                         "ideo_score",
                         "Trust_Media_w1",
                         "trust_news",
                         "trust_news_sm",
                         "cons_news_n",
                         "cons_cable",
                         "cons_print",
                         "cons_public", 
                         "cons_talk",
                         "cons_desk",
                         "cons_mobile",
                         "Safari_dummy",
                         "log_news",
                         'Prop_Unreliable_NewsG_Score',
                         'Prop_Reliable_NewsG_Score',
                         'Count_Unreliable_NewsG_Score',
                         'Count_Reliable_NewsG_Score',
                         'Average_domain_NewsG_Score',
                         'compliance_check_1',
                         'Complied',
                         "IE_dummy",
                         "Chrome_dummy",
                         "Firefox_dummy",
                         "Social_Media_Use")

top_attributes_html <- c('Gender',
                         'Education',
                         'Treatment',
                         'Age',
                         'Age-Squared',
                         "Party ID",
                         "Race/Ethnicity",
                         "Ideology ",
                         "Trust in Media",
                         "Trust of news in newspapers",
                         "Trust of news on social media ",
                         "News consumption (network news)",
                         "News consumption (cable news)",
                         "News consumption (print news) ",
                         "News consumption (public radio)", 
                         "News consumption (talk radio)",
                         "News consumption (news on desktop)",
                         "News consumption (news on mobile)",
                         "Web Browser (Safari)",
                         "Log of news viewed",
                         'Pre-Treatment Value',
                         'Pre-Treatment Value',
                         'Pre-Treatment Value',
                         'Pre-Treatment Value',
                         'Pre-Treatment Value',
                         'Treatment',
                         'Treatment',
                         "Internet Explorer Browser",
                         "Chrome Browser",
                         "Firefox Browser",
                         "Social Media use")

names(top_attributes_html) <- top_attribute_names


#Run For loop to produce Tables 3-12:


i=1
for(i in 1:length(list_variables_to_run_1)){
  list_possible_covariates_for_use <- c(list_variables_to_run_1[[i]],list_possible_covariates)
  
  data_for_analysis <- Pulse_data %>% ungroup() %>% select(`list_possible_covariates_for_use`)
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  #Use glmnet lasso to choose covariates to be a part of model:
  names_of_columns <- Lasso(data_for_analysis)
  
  #names_of_columns
  
  names_of_columns <- c('Treated',names_of_columns)
  
  
  names_use <- names(Pulse_data)[(names(Pulse_data) %in% names_of_columns)]
  
  data_for_analysis <- Pulse_data[, names_use]
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  f <- paste0(list_variables_to_run_1[[i]][1], " ~ .")
  
  ITT_Model <- lm_robust(as.formula(f), data = data_for_analysis)
  
  
  names_of_columns_2 <- c(names_of_columns,'compliance_check_1')
  names_use <- names(Pulse_data)[(names(Pulse_data) %in% names_of_columns_2)]
  data_for_analysis <- Pulse_data[, names_use]
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  f <- paste0(list_variables_to_run_1[[i]][1], '~ . - Treated | . - compliance_check_1')
  
  #CACE Model 1
  CACE_Model_1 <- iv_robust(as.formula(f), data = data_for_analysis)
  
  CACE_Model_1$term[which(CACE_Model_1$term == "compliance_check_11")] <- 'Treated'
  
  # #CACE Model 2 - Passed first and second wave compliance check
  names_of_columns_3 <- c(names_of_columns,'Complied')
  names_use <- names(Pulse_data)[(names(Pulse_data) %in% names_of_columns_3)]
  data_for_analysis <- Pulse_data[, names_use]
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  f <- paste0(list_variables_to_run_1[[i]][1], '~  . - Treated | . - Complied')
  CACE_Model_2 <- iv_robust(as.formula(f), data = data_for_analysis)
  
  CACE_Model_2$term[which(CACE_Model_2$term == "Complied")] <- 'Treated'
  
  #names of the variables:
  Variable_Names <- names(ITT_Model$coefficients)[-1]
  Variable_Names <- str_replace_all(Variable_Names, top_attributes_html)
  
  #Write Table
  texreg(list(ITT_Model,CACE_Model_1,CACE_Model_2),
         include.ci = FALSE,
         digits=4,
         omit.coef = '(Intercept)',
         caption= Titles[i],
         label = "table",
         include.rmse = FALSE,
         custom.coef.names = Variable_Names,
         custom.model.names= c("Intent-To-Treat (ITT)", "CACE (Model 1)","CACE (Model 2)"),
         file=Tables_title[i],
         float.pos = "!htbp",
         caption.above = TRUE)
}


##################################### Covariate Unadjusted #########################################################



################## Behavioral Measures (Covariate-Adjusted Models) ##############################################

#Create Sets of variables to use in models:
list_variables_to_run_1 = list(c('Prop_Unreliable_NewsG_Score_dv_dur'),
                               c('Prop_Reliable_NewsG_Score_dv_dur'),
                               c('Count_Unreliable_NewsG_Score_dv_dur'),
                               c('Count_Reliable_NewsG_Score_dv_dur'),
                               c('Average_domain_NewsG_Score_dv_dur'),
                               c('Prop_Unreliable_NewsG_Score_post_dur'),
                               c('Prop_Reliable_NewsG_Score_post_dur'),
                               c('Count_Unreliable_NewsG_Score_post_dur'),
                               c('Count_Reliable_NewsG_Score_post_dur'),
                               c('Average_domain_NewsG_Score_post_dur'))

#Create list of Titles:
Titles <- c('Testing the Effect of the Intervention on Proportion of News Diet That is Unreliable with Covariate-Adjusted Models (HC2 Robust standard errors) (Before July 1st)  -- Duration Weighted',
            'Testing the Effect of the Intervention on Proportion of News Diet That is Reliable with Covariate-Adjusted Models (HC2 Robust standard errors) (Before July 1st)  -- Duration Weighted',
            'Testing the Effect of the Intervention on Count of Unreliable News Consumed with Covariate-Adjusted Models (HC2 Robust standard errors) (Before July 1st)  -- Duration Weighted',
            'Testing the Effect of the Intervention on Count of Reliable News Consumed with Covariate-Adjusted Models (HC2 Robust standard errors) (Before July 1st)  -- Duration Weighted',
            'Testing the Effect of the Intervention on Reliability Score of News Diet with Covariate-Adjusted Models (HC2 Robust standard errors) (Before July 1st)  -- Duration Weighted',
            'Testing the Effect of the Intervention on Proportion of News Diet That is Unreliable with Covariate-Adjusted Models (HC2 Robust standard errors) (After July 1st)  -- Duration Weighted',
            'Testing the Effect of the Intervention on Proportion of News Diet That is Reliable with Covariate-Adjusted Models (HC2 Robust standard errors) (After July 1st)  -- Duration Weighted',
            'Testing the Effect of the Intervention on Count of Unreliable News Consumed with Covariate-Adjusted Models (HC2 Robust standard errors) (After July 1st)  -- Duration Weighted',
            'Testing the Effect of the Intervention on Count of Reliable News Consumed with Covariate-Adjusted Models (HC2 Robust standard errors) (After July 1st)  -- Duration Weighted',
            'Testing the Effect of the Intervention on Reliability Score of News Diet with Covariate-Adjusted Models (HC2 Robust standard errors) (After July 1st)  -- Duration Weighted')



#Create list of filenames:
Tables_title <-  c('./Tables/Table_133.txt',
                   './Tables/Table_134.txt',
                   './Tables/Table_135.txt',
                   './Tables/Table_136.txt',
                   './Tables/Table_137.txt',
                   './Tables/Table_138.txt',
                   './Tables/Table_139.txt',
                   './Tables/Table_140.txt',
                   './Tables/Table_141.txt',
                   './Tables/Table_142.txt')

#Create crosswalk for variables and display names:
top_attribute_names <- c('Prop_Unreliable_NewsG_Score_dur',
                         'Prop_Reliable_NewsG_Score_dur',
                         'Count_Unreliable_NewsG_Score_dur',
                         'Count_Reliable_NewsG_Score_dur',
                         'Average_domain_NewsG_Score_dur')

top_attributes_html <- c('Pre-Treatment Value',
                         'Pre-Treatment Value',
                         'Pre-Treatment Value',
                         'Pre-Treatment Value',
                         'Pre-Treatment Value')

names(top_attributes_html) <- top_attribute_names


#Run For loop to produce Tables 3-12:


i=1
for(i in 1:length(list_variables_to_run_1)){
  list_possible_covariates_for_use <- c('Treated',list_variables_to_run_1[[i]])
  data_for_analysis <- Pulse_data %>% ungroup() %>% select(`list_possible_covariates_for_use`)
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  f <- paste0(list_variables_to_run_1[[i]][1], " ~ .")
  
  ITT_Model <- lm_robust(as.formula(f), data = data_for_analysis)
  
  names_of_columns_2 <- c('Treated',list_possible_covariates_for_use,'compliance_check_1')
  
  names_use <- names(Pulse_data)[(names(Pulse_data) %in% names_of_columns_2)]
  
  data_for_analysis <- Pulse_data[, names_use]
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  f <- paste0(list_variables_to_run_1[[i]][1], '~ . - Treated | . - compliance_check_1')
  
  #CACE Model 1
  CACE_Model_1 <- iv_robust(as.formula(f), data = data_for_analysis)
  
  CACE_Model_1$term[which(CACE_Model_1$term == "compliance_check_11")] <- 'Treated'
  
  # #CACE Model 2 - Passed first and second wave compliance check
  
  names_of_columns_3 <- c('Treated',list_possible_covariates_for_use,'Complied')
  
  names_use <- names(Pulse_data)[(names(Pulse_data) %in% names_of_columns_3)]
  
  data_for_analysis <- Pulse_data[, names_use]
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  f <- paste0(list_variables_to_run_1[[i]][1], '~  . - Treated | . - Complied')
  
  CACE_Model_2 <- iv_robust(as.formula(f), data = data_for_analysis)
  
  CACE_Model_2$term[which(CACE_Model_2$term == "Complied")] <- 'Treated'
  
  
  #names of the variables:
  
  Variable_Names <- names(ITT_Model$coefficients)[-1]
  Variable_Names <- str_replace_all(Variable_Names, top_attributes_html)
  
  #Write Table
  texreg(list(ITT_Model,CACE_Model_1,CACE_Model_2),
         include.ci = FALSE,
         digits=4,
         omit.coef = '(Intercept)',
         caption= Titles[i],
         label = "table",
         include.rmse = FALSE,
         custom.coef.names = Variable_Names,
         custom.model.names= c("Intent-To-Treat (ITT)", "CACE (Model 1)","CACE (Model 2)"),
         file=Tables_title[i],
         float.pos = "!htbp",
         caption.above = TRUE)
}



################## Behavioral Measures (Covariate-Adjusted Models) - Referral ##############################################

#List of possible variables for inclusion:
list_possible_covariates <- c("gender_dummy_fem",
                              "educ_score",
                              "Age",
                              "Age_Sq",
                              "party_score",
                              "race_white",
                              "ideo_score",
                              "Trust_Media_w1",
                              "trust_news",
                              "trust_news_sm",
                              "cons_news_n",
                              "cons_cable",
                              "cons_print",
                              "cons_public", 
                              "cons_talk",
                              "cons_desk",
                              "cons_mobile",
                              "log_news",
                              "Safari_dummy",
                              "IE_dummy",
                              "Chrome_dummy",
                              "Firefox_dummy",
                              "Social_Media_Use")

#Create Sets of variables to use in models:
list_variables_to_run_1 = list(c('Prop_Unreliable_NewsG_Score_dv_All_ref'),
                               c('Prop_Reliable_NewsG_Score_dv_All_ref'),
                               c('Count_Unreliable_NewsG_Score_dv_All_ref'),
                               c('Count_Reliable_NewsG_Score_dv_All_ref'),
                               c('Average_domain_NewsG_Score_dv_All_ref'),
                               c('Prop_Unreliable_NewsG_Score_post_All_ref'),
                               c('Prop_Reliable_NewsG_Score_post_All_ref'),
                               c('Count_Unreliable_NewsG_Score_post_All_ref'),
                               c('Count_Reliable_NewsG_Score_post_All_ref'),
                               c('Average_domain_NewsG_Score_post_All_ref'))




#Create list of Titles:
Titles <- c('Testing the Effect of the Intervention on Proportion of News Diet of Referrals From Search Engines and Social Media (Google, Twitter, and Facebook) That is Unreliable with Covariate-Adjusted Models (HC2 Robust standard errors) (Before July 1st)',
            'Testing the Effect of the Intervention on Proportion of News Diet of Referrals From Search Engines and Social Media (Google, Twitter, and Facebook) That is Reliable with Covariate-Adjusted Models (HC2 Robust standard errors) (Before July 1st)',
            'Testing the Effect of the Intervention on Count of Unreliable News Referred From Search Engines and Social Media (Google, Twitter, and Facebook) with Covariate-Adjusted Models (HC2 Robust standard errors) (Before July 1st)',
            'Testing the Effect of the Intervention on Count of Reliable News Referred From Search Engines and Social Media (Google, Twitter, and Facebook) with Covariate-Adjusted Models (HC2 Robust standard errors) (Before July 1st)',
            'Testing the Effect of the Intervention on Reliability Score of News Referred From Search Engines and Social Media (Google, Twitter, and Facebook) with Covariate-Adjusted Models (HC2 Robust standard errors) (Before July 1st)',
            'Testing the Effect of the Intervention on Proportion of News Diet of Referrals From Search Engines and Social Media (Google, Twitter, and Facebook) That is Unreliable with Covariate-Adjusted Models (HC2 Robust standard errors) (After July 1st)',
            'Testing the Effect of the Intervention on Proportion News Diet of Referrals From Search Engines and Social Media (Google, Twitter, and Facebook) That is Reliable with Covariate-Adjusted Models (HC2 Robust standard errors) (After July 1st)',
            'Testing the Effect of the Intervention on Count of Unreliable News Referred From Search Engines and Social Media (Google, Twitter, and Facebook) with Covariate-Adjusted Models (HC2 Robust standard errors) (After July 1st)',
            'Testing the Effect of the Intervention on Count of Reliable News Referred From Search Engines and Social Media (Google, Twitter, and Facebook) with Covariate-Adjusted Models (HC2 Robust standard errors) (After July 1st)',
            'Testing the Effect of the Intervention on Reliability Score of News Referred From Search Engines and Social Media (Google, Twitter, and Facebook) with Covariate-Adjusted Models (HC2 Robust standard errors) (After July 1st)')



#Create list of filenames:
Tables_title <-  c('./Tables/Table_153.txt',
                   './Tables/Table_154.txt',
                   './Tables/Table_155.txt',
                   './Tables/Table_156.txt',
                   './Tables/Table_157.txt',
                   './Tables/Table_158.txt',
                   './Tables/Table_159.txt',
                   './Tables/Table_150.txt',
                   './Tables/Table_151.txt',
                   './Tables/Table_152.txt')

#Create crosswalk for variables and display names:
top_attribute_names <- c('Prop_Unreliable_NewsG_Score_All_ref',
                         'Prop_Reliable_NewsG_Score_All_ref',
                         'Count_Unreliable_NewsG_Score_All_ref',
                         'Count_Reliable_NewsG_Score_All_ref',
                         'Average_domain_NewsG_Score_All_ref')

top_attributes_html <- c('Pre-Treatment Value',
                         'Pre-Treatment Value',
                         'Pre-Treatment Value',
                         'Pre-Treatment Value',
                         'Pre-Treatment Value')

names(top_attributes_html) <- top_attribute_names


#Run For loop to produce Tables 3-12:


i=1
for(i in 1:length(list_variables_to_run_1)){
  list_possible_covariates_for_use <- c('Treated',list_variables_to_run_1[[i]])
  data_for_analysis <- Pulse_data %>% ungroup() %>% select(`list_possible_covariates_for_use`)
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  f <- paste0(list_variables_to_run_1[[i]][1], " ~ .")
  
  ITT_Model <- lm_robust(as.formula(f), data = data_for_analysis)
  
  names_of_columns_2 <- c('Treated',list_possible_covariates_for_use,'compliance_check_1')
  
  names_use <- names(Pulse_data)[(names(Pulse_data) %in% names_of_columns_2)]
  
  data_for_analysis <- Pulse_data[, names_use]
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  f <- paste0(list_variables_to_run_1[[i]][1], '~ . - Treated | . - compliance_check_1')
  
  #CACE Model 1
  CACE_Model_1 <- iv_robust(as.formula(f), data = data_for_analysis)
  
  CACE_Model_1$term[which(CACE_Model_1$term == "compliance_check_11")] <- 'Treated'
  
  # #CACE Model 2 - Passed first and second wave compliance check
  
  names_of_columns_3 <- c('Treated',list_possible_covariates_for_use,'Complied')
  
  names_use <- names(Pulse_data)[(names(Pulse_data) %in% names_of_columns_3)]
  
  data_for_analysis <- Pulse_data[, names_use]
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  f <- paste0(list_variables_to_run_1[[i]][1], '~  . - Treated | . - Complied')
  
  CACE_Model_2 <- iv_robust(as.formula(f), data = data_for_analysis)
  
  CACE_Model_2$term[which(CACE_Model_2$term == "Complied")] <- 'Treated'
  
  
  #names of the variables:
  
  Variable_Names <- names(ITT_Model$coefficients)[-1]
  Variable_Names <- str_replace_all(Variable_Names, top_attributes_html)
  
  #Write Table
  texreg(list(ITT_Model,CACE_Model_1,CACE_Model_2),
         include.ci = FALSE,
         digits=4,
         omit.coef = '(Intercept)',
         caption= Titles[i],
         label = "table",
         include.rmse = FALSE,
         custom.coef.names = Variable_Names,
         custom.model.names= c("Intent-To-Treat (ITT)", "CACE (Model 1)","CACE (Model 2)"),
         file=Tables_title[i],
         float.pos = "!htbp",
         caption.above = TRUE)
}

################## Behavioral Measures (Covariate-Adjusted Models) - Referral ##############################################


#Create Sets of variables to use in models:
list_variables_to_run_1 = list(c('Prop_Unreliable_NewsG_Score_dv_SM_ref'),
                               c('Prop_Reliable_NewsG_Score_dv_SM_ref'),
                               c('Count_Unreliable_NewsG_Score_dv_SM_ref'),
                               c('Count_Reliable_NewsG_Score_dv_SM_ref'),
                               c('Average_domain_NewsG_Score_dv_SM_ref'),
                               c('Prop_Unreliable_NewsG_Score_post_SM_ref'),
                               c('Prop_Reliable_NewsG_Score_post_SM_ref'),
                               c('Count_Unreliable_NewsG_Score_post_SM_ref'),
                               c('Count_Reliable_NewsG_Score_post_SM_ref'),
                               c('Average_domain_NewsG_Score_post_SM_ref'))

#Create list of Titles:
Titles <- c('Testing the Effect of the Intervention on Proportion of News Diet of Referrals From Social Media (Twitter and Facebook) That is Unreliable with Covariate-Adjusted Models (HC2 Robust standard errors) (Before July 1st)',
            'Testing the Effect of the Intervention on Proportion of News Diet of Referrals From Social Media (Twitter and Facebook) That is Reliable with Covariate-Adjusted Models (HC2 Robust standard errors) (Before July 1st)',
            'Testing the Effect of the Intervention on Count of Unreliable News Referred From Social Media (Twitter and Facebook) with Covariate-Adjusted Models (HC2 Robust standard errors) (Before July 1st)',
            'Testing the Effect of the Intervention on Count of Reliable News Referred From Social Media (Twitter and Facebook) with Covariate-Adjusted Models (HC2 Robust standard errors) (Before July 1st)',
            'Testing the Effect of the Intervention on Reliability Score of News Referred From Social Media (Twitter and Facebook) with Covariate-Adjusted Models (HC2 Robust standard errors) (Before July 1st)',
            'Testing the Effect of the Intervention on Proportion of News Diet of Referrals From Social Media (Twitter and Facebook) That is Unreliable with Covariate-Adjusted Models (HC2 Robust standard errors) (After July 1st)',
            'Testing the Effect of the Intervention on Proportion News Diet of Referrals From Social Media (Twitter and Facebook) That is Reliable with Covariate-Adjusted Models (HC2 Robust standard errors) (After July 1st)',
            'Testing the Effect of the Intervention on Count of Unreliable News Referred From Social Media (Twitter and Facebook) with Covariate-Adjusted Models (HC2 Robust standard errors) (After July 1st)',
            'Testing the Effect of the Intervention on Count of Reliable News Referred From Social Media (Twitter and Facebook) with Covariate-Adjusted Models (HC2 Robust standard errors) (After July 1st)',
            'Testing the Effect of the Intervention on Reliability Score of News Referred From Social Media (Twitter and Facebook) with Covariate-Adjusted Models (HC2 Robust standard errors) (After July 1st)')



#Create list of filenames:
Tables_title <-  c('./Tables/Table_143.txt',
                   './Tables/Table_144.txt',
                   './Tables/Table_145.txt',
                   './Tables/Table_146.txt',
                   './Tables/Table_147.txt',
                   './Tables/Table_148.txt',
                   './Tables/Table_149.txt',
                   './Tables/Table_150.txt',
                   './Tables/Table_151.txt',
                   './Tables/Table_152.txt')

#Create crosswalk for variables and display names:
top_attribute_names <- c("gender_dummy_fem",
                         "educ_score",
                         'Treated',
                         "Age",
                         "Age_Sq",
                         "party_score",
                         "race_white",
                         "ideo_score",
                         "Trust_Media_w1",
                         "trust_news",
                         "trust_news_sm",
                         "cons_news_n",
                         "cons_cable",
                         "cons_print",
                         "cons_public", 
                         "cons_talk",
                         "cons_desk",
                         "cons_mobile",
                         "Safari_dummy",
                         "log_news",
                         'Prop_Unreliable_NewsG_Score',
                         'Prop_Reliable_NewsG_Score',
                         'Count_Unreliable_NewsG_Score',
                         'Count_Reliable_NewsG_Score',
                         'Average_domain_NewsG_Score',
                         'compliance_check_1',
                         'Complied',
                         "IE_dummy",
                         "Chrome_dummy",
                         "Firefox_dummy",
                         "Social_Media_Use")

top_attributes_html <- c('Gender',
                         'Education',
                         'Treatment',
                         'Age',
                         'Age-Squared',
                         "Party ID",
                         "Race/Ethnicity",
                         "Ideology ",
                         "Trust in Media",
                         "Trust of news in newspapers",
                         "Trust of news on social media ",
                         "News consumption (network news)",
                         "News consumption (cable news)",
                         "News consumption (print news) ",
                         "News consumption (public radio)", 
                         "News consumption (talk radio)",
                         "News consumption (news on desktop)",
                         "News consumption (news on mobile)",
                         "Web Browser (Safari)",
                         "Log of news viewed",
                         'Pre-Treatment Value',
                         'Pre-Treatment Value',
                         'Pre-Treatment Value',
                         'Pre-Treatment Value',
                         'Pre-Treatment Value',
                         'Treatment',
                         'Treatment',
                         "Internet Explorer Browser",
                         "Chrome Browser",
                         "Firefox Browser",
                         "Social Media use")

names(top_attributes_html) <- top_attribute_names


#Run For loop to produce Tables 3-12:


i=1
for(i in 1:length(list_variables_to_run_1)){
  list_possible_covariates_for_use <- c('Treated',list_variables_to_run_1[[i]])
  data_for_analysis <- Pulse_data %>% ungroup() %>% select(`list_possible_covariates_for_use`)
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  f <- paste0(list_variables_to_run_1[[i]][1], " ~ .")
  
  ITT_Model <- lm_robust(as.formula(f), data = data_for_analysis)
  
  names_of_columns_2 <- c('Treated',list_possible_covariates_for_use,'compliance_check_1')
  
  names_use <- names(Pulse_data)[(names(Pulse_data) %in% names_of_columns_2)]
  
  data_for_analysis <- Pulse_data[, names_use]
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  f <- paste0(list_variables_to_run_1[[i]][1], '~ . - Treated | . - compliance_check_1')
  
  #CACE Model 1
  CACE_Model_1 <- iv_robust(as.formula(f), data = data_for_analysis)
  
  CACE_Model_1$term[which(CACE_Model_1$term == "compliance_check_11")] <- 'Treated'
  
  # #CACE Model 2 - Passed first and second wave compliance check
  
  names_of_columns_3 <- c('Treated',list_possible_covariates_for_use,'Complied')
  
  names_use <- names(Pulse_data)[(names(Pulse_data) %in% names_of_columns_3)]
  
  data_for_analysis <- Pulse_data[, names_use]
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  f <- paste0(list_variables_to_run_1[[i]][1], '~  . - Treated | . - Complied')
  
  CACE_Model_2 <- iv_robust(as.formula(f), data = data_for_analysis)
  
  CACE_Model_2$term[which(CACE_Model_2$term == "Complied")] <- 'Treated'
  
  
  #names of the variables:
  
  Variable_Names <- names(ITT_Model$coefficients)[-1]
  Variable_Names <- str_replace_all(Variable_Names, top_attributes_html)
  
  #Write Table
  texreg(list(ITT_Model,CACE_Model_1,CACE_Model_2),
         include.ci = FALSE,
         digits=4,
         omit.coef = '(Intercept)',
         caption= Titles[i],
         label = "table",
         include.rmse = FALSE,
         custom.coef.names = Variable_Names,
         custom.model.names= c("Intent-To-Treat (ITT)", "CACE (Model 1)","CACE (Model 2)"),
         file=Tables_title[i],
         float.pos = "!htbp",
         caption.above = TRUE)
}





