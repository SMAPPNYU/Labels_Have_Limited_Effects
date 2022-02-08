
########################################################################################################################################################
### Title: News Credibility Labels Improve News Diets, Reduce Misperceptions, and Increase Media Trust
### Authors: Kevin Aslett, Andrew Guess, Jonathan Nagler, Richard Bonneaua, and Joshua A. Tucker
### Purpose of code: Produce figures and tables of analyses that are displayed in the main test of the paper

##### Data In:
# (1) ./Data/Clean_NewsGuard_Digital_Trace_Data.csv
# (2) ./Data/Clean_NewsGuard_Survey_Study.csv

##### Data Out: NONE

########################################################################################################################################################

#Load Libraries:
library(tidyverse)
library(dplyr)
library(AER)
library(glmnet)
library(estimatr)
library(huxtable)
library(magrittr)
library(texreg)
library(ivpack)


#Read in survey data with digital trace data:
Pulse_data <- read_csv('./Data/Clean_NewsGuard_Digital_Trace_Data.csv',
               col_types = cols(
               .default= col_character(),
               Treated = col_double(),
               Complied=col_double(),
               IE_dummy = col_double(),
               Chrome_dummy = col_double(),
               Firefox_dummy = col_double(),
               Social_Media_Use = col_double(),
               compliance_check_1=col_double(),
               Average_domain_NewsG_Score_post= col_double(),
               Average_domain_NewsG_Score= col_double(),
               Prop_Unreliable_NewsG_Score_post= col_double(),
               Prop_Unreliable_NewsG_Score= col_double(),
               Prop_Unreliable_NewsG_Score_post = col_double(),
               Prop_Unreliable_NewsG_Score = col_double(),
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
               gender_dummy_fem=col_double(),
               educ_score=col_double(),
               Age=col_double(),
               Age_Sq=col_double(),
               party_score=col_double(),
               race_white=col_double(),
               ideo_score=col_double(),
               Trust_Media_w1=col_double(),
               Trust_Media_w2=col_double(),
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
               log_news=col_double()
               ))

#Read in survey data without digital trace data:

data_frame_1 <- read_csv('./Data/Clean_NewsGuard_Survey_Study.csv',
                         col_types = cols(
                         .default= col_character(),
                         IE_dummy = col_double(),
                         Chrome_dummy = col_double(),
                         Firefox_dummy = col_double(),
                         Social_Media_Use = col_double(),
                         Treated = col_double(),
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
                         Complied=col_double(),
                         compliance_check_1=col_double(),
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
                         Safari_dummy=col_double()
                         ))

#Ensure that the order of the dataframe is set:
data_frame_1 <- data_frame_1[order(data_frame_1$visa1),]


#Create a function using glmnet lasso that chooses the variables to use:
Lasso <- function(data_for_analysis) {
  set.seed(938)
  lasso_select <- cv.glmnet(x=as.matrix(data_for_analysis[,-1]),
                            y=as.vector(data_for_analysis[,1]),
                            alpha=1)
  
  coef.out <- coef(lasso_select, exact = TRUE)
  indices <- which(coef.out != 0)
  names_of_columns_3 <- c(rownames(coef.out)[indices],colnames(data_for_analysis)[1])
  names_of_columns_3 <- names_of_columns_3[!names_of_columns_3 %in% "(Intercept)"]
  return(names_of_columns_3)
    }

#Function for cleaning data:

Clean <- function(data_for_analysis) {
#Replace Infinite values in data with NA
data_for_analysis <- do.call(data.frame,                      
                             lapply(data_for_analysis,
                                    function(x) replace(x, is.infinite(x), NA)))
#Remove NA values:
data_for_analysis <- na.omit(data_for_analysis)

return(data_for_analysis)
}

######### Run Covariate-Adjusted Complier Average Causal Effects (CACE) using strongest compliance check:

### Test Hypotheses 1, 2, and 3:
#(H1) We test whether in-feed source reliability labels shift downstream news and information consumption from unreliable sources known for publishing misleading or false content to more reliable sources
#(H2) We test whether it increases trust in mainstream media and reliable sources (H2), and  
#(H3) mitigate phenomena associated with democratic dysfunction (affective polarization and political cynicism). 

######################### (Hypothesis 1) ################################

################## Proportion Unreliable (After July 1st) ##############################################

data_for_analysis <- Pulse_data %>% ungroup() %>% select(Prop_Unreliable_NewsG_Score_post,
                                                         Prop_Unreliable_NewsG_Score,
                                                         gender_dummy_fem,
                                                         educ_score,
                                                         Age,
                                                         Age_Sq,
                                                         party_score,
                                                         race_white,
                                                         ideo_score,
                                                         Trust_Media_w1,
                                                         trust_news,
                                                         trust_news_sm,
                                                         cons_news_n,
                                                         cons_cable,
                                                         cons_print,
                                                         cons_public, 
                                                         cons_talk,
                                                         cons_desk,
                                                         cons_mobile,
                                                         Safari_dummy,
                                                         log_news,
                                                         IE_dummy,
                                                         Chrome_dummy,
                                                         Firefox_dummy,
                                                         Social_Media_Use)



#Clean Data:
data_for_analysis <- Clean(data_for_analysis)

#Use glmnet lasso to choose covariates to be a part of model:
names_of_columns_3 <- Lasso(data_for_analysis)

names_of_columns_3 <- c('Treated',names_of_columns_3)

names.use <- names(Pulse_data)[(names(Pulse_data) %in% names_of_columns_3)]

data_for_analysis <- Pulse_data[, names.use]

data_for_analysis <- do.call(data.frame,                      # Replace Inf in data by NA
                             lapply(data_for_analysis,
                                    function(x) replace(x, is.infinite(x), NA)))

ivreg_Unrel_Post_compl_2 <- lm_robust(Prop_Unreliable_NewsG_Score_post ~ ., data = data_for_analysis)


################## Domain Score (After July 1st)   ##############################################
data_for_analysis <- Pulse_data %>%  ungroup() %>% select(Average_domain_NewsG_Score_post,
                                                          Average_domain_NewsG_Score,
                                                          gender_dummy_fem,
                                                          educ_score,
                                                          Age,
                                                          Age_Sq,
                                                          party_score,
                                                          race_white,
                                                          ideo_score,
                                                          Trust_Media_w1,
                                                          trust_news,
                                                          trust_news_sm,
                                                          cons_news_n,
                                                          cons_cable,
                                                          cons_print,
                                                          cons_public, 
                                                          cons_talk,
                                                          cons_desk,
                                                          cons_mobile,
                                                          Safari_dummy,
                                                          log_news)


#Clean Data:
data_for_analysis <- Clean(data_for_analysis)

#Use glmnet lasso to choose covariates to be a part of model:
names_of_columns_3 <- Lasso(data_for_analysis)

names_of_columns_3 <- c('Treated',names_of_columns_3)

names.use <- names(Pulse_data)[(names(Pulse_data) %in% names_of_columns_3)]

data_for_analysis <- Pulse_data[, names.use]

data_for_analysis <- do.call(data.frame,                      # Replace Inf in data by NA
                             lapply(data_for_analysis,
                                    function(x) replace(x, is.infinite(x), NA)))

ivreg_news_Post_compl_2 <- lm_robust(Average_domain_NewsG_Score_post ~ ., data = data_for_analysis)

################## Proportion Reliable (After July 1st)   ##############################################
data_for_analysis <- Pulse_data %>% ungroup() %>%  select(Prop_Reliable_NewsG_Score_post,
                                                          Prop_Reliable_NewsG_Score,
                                                          gender_dummy_fem,
                                                          educ_score,
                                                          Age,
                                                          Age_Sq,
                                                          party_score,
                                                          race_white,
                                                          ideo_score,
                                                          Trust_Media_w1,
                                                          trust_news,
                                                          trust_news_sm,
                                                          cons_news_n,
                                                          cons_cable,
                                                          cons_print,
                                                          cons_public, 
                                                          cons_talk,
                                                          cons_desk,
                                                          cons_mobile,
                                                          Safari_dummy,
                                                          log_news,
                                                          IE_dummy,
                                                          Chrome_dummy,
                                                          Firefox_dummy,
                                                          Social_Media_Use)

#Clean Data:
data_for_analysis <- Clean(data_for_analysis)

#Create list of covariates that should be included:
names_of_columns_3 <- Lasso(data_for_analysis)

names_of_columns_3 <- c('Treated',names_of_columns_3)

names.use <- names(Pulse_data)[(names(Pulse_data) %in% names_of_columns_3)]

data_for_analysis <- Pulse_data[, names.use]

data_for_analysis <- do.call(data.frame,                      # Replace Inf in data by NA
                             lapply(data_for_analysis,
                                    function(x) replace(x, is.infinite(x), NA)))

ivreg_Rel_Post_compl_2 <- lm_robust(Prop_Reliable_NewsG_Score_post ~ ., data = data_for_analysis)


################## Count of Unreliable (After July 1st)   ##############################################
data_for_analysis <- Pulse_data %>% ungroup() %>%  select(Count_Unreliable_NewsG_Score_post,
                                                          Count_Unreliable_NewsG_Score,
                                                          gender_dummy_fem,
                                                          educ_score,
                                                          Age,
                                                          Age_Sq,
                                                          party_score,
                                                          race_white,
                                                          ideo_score,
                                                          Trust_Media_w1,
                                                          trust_news,
                                                          trust_news_sm,
                                                          cons_news_n,
                                                          cons_cable,
                                                          cons_print,
                                                          cons_public, 
                                                          cons_talk,
                                                          cons_desk,
                                                          cons_mobile,
                                                          Safari_dummy,
                                                          log_news,
                                                          IE_dummy,
                                                          Chrome_dummy,
                                                          Firefox_dummy,
                                                          Social_Media_Use)

#Clean Data:
data_for_analysis <- Clean(data_for_analysis)

#Use glmnet lasso to choose covariates to be a part of model:
names_of_columns_3 <- Lasso(data_for_analysis)

names_of_columns_3 <- c('Treated',names_of_columns_3)

names.use <- names(Pulse_data)[(names(Pulse_data) %in% names_of_columns_3)]

data_for_analysis <- Pulse_data[, names.use]

data_for_analysis <- do.call(data.frame,                      # Replace Inf in data by NA
                             lapply(data_for_analysis,
                                    function(x) replace(x, is.infinite(x), NA)))

ivreg_Unrel_c_Post_compl_2 <- lm_robust(Count_Unreliable_NewsG_Score_post ~ ., data = data_for_analysis)


################## Count of Reliable (After July 1st)  ##############################################
data_for_analysis <- Pulse_data %>% ungroup() %>%  select(Count_Reliable_NewsG_Score_post,
                                                          Count_Reliable_NewsG_Score,
                                                          gender_dummy_fem,
                                                          educ_score,
                                                          Age,
                                                          Age_Sq,
                                                          party_score,
                                                          race_white,
                                                          ideo_score,
                                                          Trust_Media_w1,
                                                          trust_news,
                                                          trust_news_sm,
                                                          cons_news_n,
                                                          cons_cable,
                                                          cons_print,
                                                          cons_public, 
                                                          cons_talk,
                                                          cons_desk,
                                                          cons_mobile,
                                                          Safari_dummy,
                                                          log_news,
                                                          IE_dummy,
                                                          Chrome_dummy,
                                                          Firefox_dummy,
                                                          Social_Media_Use)

#Clean Data:
data_for_analysis <- Clean(data_for_analysis)

#Use glmnet lasso to choose covariates to be a part of model:
names_of_columns_3 <- Lasso(data_for_analysis)

names_of_columns_3 <- c('Treated',names_of_columns_3)

names.use <- names(Pulse_data)[(names(Pulse_data) %in% names_of_columns_3)]

data_for_analysis <- Pulse_data[, names.use]

data_for_analysis <- do.call(data.frame,                      # Replace Inf in data by NA
                             lapply(data_for_analysis,
                                    function(x) replace(x, is.infinite(x), NA)))

ivreg_rel_c_Post_compl_2 <- lm_robust(Count_Reliable_NewsG_Score_post ~ ., data = data_for_analysis)

###############Comparing pre-treatment period to the period between Wave 1 survey and July 1st (Treatment Period): 

################## Proportion Unreliable (Treatment Period)  ##############################################

data_for_analysis <- Pulse_data %>% ungroup() %>%  select(Prop_Unreliable_NewsG_Score_dv,
                                                          Prop_Unreliable_NewsG_Score,
                                                          gender_dummy_fem,
                                                          educ_score,
                                                          Age,
                                                          Age_Sq,
                                                          party_score,
                                                          race_white,
                                                          ideo_score,
                                                          Trust_Media_w1,
                                                          trust_news,
                                                          trust_news_sm,
                                                          cons_news_n,
                                                          cons_cable,
                                                          cons_print,
                                                          cons_public, 
                                                          cons_talk,
                                                          cons_desk,
                                                          cons_mobile,
                                                          Safari_dummy,
                                                          log_news,
                                                          IE_dummy,
                                                          Chrome_dummy,
                                                          Firefox_dummy,
                                                          Social_Media_Use)

#Clean Data:
data_for_analysis <- Clean(data_for_analysis)

#Use glmnet lasso to choose covariates to be a part of model:
names_of_columns_3 <- Lasso(data_for_analysis)

names_of_columns_3 <- c('Treated',names_of_columns_3)

names.use <- names(Pulse_data)[(names(Pulse_data) %in% names_of_columns_3)]

data_for_analysis <- Pulse_data[, names.use]

data_for_analysis <- do.call(data.frame,                      # Replace Inf in data by NA
                             lapply(data_for_analysis,
                                    function(x) replace(x, is.infinite(x), NA)))

ivreg_Unrel_dv_compl_2 <- lm_robust(Prop_Unreliable_NewsG_Score_dv ~ ., data = data_for_analysis)

################## Domain Score (Treatment Period)  ##############################################
data_for_analysis <- Pulse_data %>%  ungroup() %>%  select(Average_domain_NewsG_Score_dv,
                                                           Average_domain_NewsG_Score,
                                                           gender_dummy_fem,
                                                           educ_score,
                                                           Age,
                                                           Age_Sq,
                                                           party_score,
                                                           race_white,
                                                           ideo_score,
                                                           Trust_Media_w1,
                                                           trust_news,
                                                           trust_news_sm,
                                                           cons_news_n,
                                                           cons_cable,
                                                           cons_print,
                                                           cons_public, 
                                                           cons_talk,
                                                           cons_desk,
                                                           cons_mobile,
                                                           Safari_dummy,
                                                           log_news,
                                                           IE_dummy,
                                                           Chrome_dummy,
                                                           Firefox_dummy,
                                                           Social_Media_Use)

#Clean Data:
data_for_analysis <- Clean(data_for_analysis)

#Use glmnet lasso to choose covariates to be a part of model:
names_of_columns_3 <- Lasso(data_for_analysis)

names_of_columns_3 <- c('Treated',names_of_columns_3)

names.use <- names(Pulse_data)[(names(Pulse_data) %in% names_of_columns_3)]

data_for_analysis <- Pulse_data[, names.use]

data_for_analysis <- do.call(data.frame,                      # Replace Inf in data by NA
                             lapply(data_for_analysis,
                                    function(x) replace(x, is.infinite(x), NA)))

ivreg_news_dv_compl_2 <- lm_robust(Average_domain_NewsG_Score_dv ~ ., data = data_for_analysis)

################## Proportion Reliable (Treatment Period)  ##############################################

data_for_analysis <- Pulse_data %>% ungroup() %>%  select(Prop_Reliable_NewsG_Score_dv,
                                                          Prop_Reliable_NewsG_Score,
                                                          gender_dummy_fem,
                                                          educ_score,
                                                          Age,
                                                          Age_Sq,
                                                          party_score,
                                                          race_white,
                                                          ideo_score,
                                                          Trust_Media_w1,
                                                          trust_news,
                                                          trust_news_sm,
                                                          cons_news_n,
                                                          cons_cable,
                                                          cons_print,
                                                          cons_public, 
                                                          cons_talk,
                                                          cons_desk,
                                                          cons_mobile,
                                                          Safari_dummy,
                                                          log_news,
                                                          IE_dummy,
                                                          Chrome_dummy,
                                                          Firefox_dummy,
                                                          Social_Media_Use)

#Clean Data:
data_for_analysis <- Clean(data_for_analysis)

#Use glmnet lasso to choose covariates to be a part of model:
names_of_columns_3 <- Lasso(data_for_analysis)

names_of_columns_3 <- c('Treated',names_of_columns_3)

names.use <- names(Pulse_data)[(names(Pulse_data) %in% names_of_columns_3)]

data_for_analysis <- Pulse_data[, names.use]

data_for_analysis <- do.call(data.frame,                      # Replace Inf in data by NA
                             lapply(data_for_analysis,
                                    function(x) replace(x, is.infinite(x), NA)))

ivreg_Rel_dv_compl_2 <- lm_robust(Prop_Reliable_NewsG_Score_dv ~ ., data = data_for_analysis)


################## Count of Unreliable (Treatment Period)  ##############################################

data_for_analysis <- Pulse_data %>% ungroup() %>%  select(Count_Unreliable_NewsG_Score_dv,
                                                          Count_Unreliable_NewsG_Score,
                                                          gender_dummy_fem,
                                                          educ_score,
                                                          Age,
                                                          Age_Sq,
                                                          party_score,
                                                          race_white,
                                                          ideo_score,
                                                          Trust_Media_w1,
                                                          trust_news,
                                                          trust_news_sm,
                                                          cons_news_n,
                                                          cons_cable,
                                                          cons_print,
                                                          cons_public, 
                                                          cons_talk,
                                                          cons_desk,
                                                          cons_mobile,
                                                          Safari_dummy,
                                                          log_news,
                                                          IE_dummy,
                                                          Chrome_dummy,
                                                          Firefox_dummy,
                                                          Social_Media_Use)

#Clean Data:
data_for_analysis <- Clean(data_for_analysis)

#Use glmnet lasso to choose covariates to be a part of model:
names_of_columns_3 <- Lasso(data_for_analysis)

names_of_columns_3 <- c('Treated',names_of_columns_3)

names.use <- names(Pulse_data)[(names(Pulse_data) %in% names_of_columns_3)]

data_for_analysis <- Pulse_data[, names.use]

data_for_analysis <- do.call(data.frame,                      # Replace Inf in data by NA
                             lapply(data_for_analysis,
                                    function(x) replace(x, is.infinite(x), NA)))

ivreg_Unrel_c_dv_compl_2 <- lm_robust(Count_Unreliable_NewsG_Score_dv ~ ., data = data_for_analysis)


################## Count of Reliable (Treatment Period) ##############################################

data_for_analysis <- Pulse_data %>% ungroup() %>%  select(Count_Reliable_NewsG_Score_dv,
                                                          Count_Reliable_NewsG_Score,
                                                          gender_dummy_fem,
                                                          educ_score,
                                                          Age,
                                                          Age_Sq,
                                                          party_score,
                                                          race_white,
                                                          ideo_score,
                                                          Trust_Media_w1,
                                                          trust_news,
                                                          trust_news_sm,
                                                          cons_news_n,
                                                          cons_cable,
                                                          cons_print,
                                                          cons_public, 
                                                          cons_talk,
                                                          cons_desk,
                                                          cons_mobile,
                                                          Safari_dummy,
                                                          log_news,
                                                          IE_dummy,
                                                          Chrome_dummy,
                                                          Firefox_dummy,
                                                          Social_Media_Use)

#Clean Data:
data_for_analysis <- Clean(data_for_analysis)

#Use glmnet lasso to choose covariates to be a part of model:
names_of_columns_3 <- Lasso(data_for_analysis)

names_of_columns_3 <- c('Treated',names_of_columns_3)

names.use <- names(Pulse_data)[(names(Pulse_data) %in% names_of_columns_3)]

data_for_analysis <- Pulse_data[, names.use]

data_for_analysis <- do.call(data.frame,                      # Replace Inf in data by NA
                             lapply(data_for_analysis,
                                    function(x) replace(x, is.infinite(x), NA)))

ivreg_rel_c_dv_compl_2 <- lm_robust(Count_Reliable_NewsG_Score_dv ~ ., data = data_for_analysis)


################################# Hypothesis 2a: Trust in General Media ################################
#Using survey data create new data_frame with only possible variables included in the model:

data_for_analysis <- data_frame_1 %>% ungroup() %>% select(Trust_Media_w2,
                                                          Trust_Media_w1,
                                                          gender_dummy_fem,
                                                          educ_score,
                                                          Age,
                                                          Age_Sq,
                                                          party_score,
                                                          race_white,
                                                          ideo_score,
                                                          trust_news,
                                                          trust_news_sm,
                                                          cons_news_n,
                                                          cons_cable,
                                                          cons_print,
                                                          cons_public, 
                                                          cons_talk,
                                                          cons_desk,
                                                          cons_mobile,
                                                          Safari_dummy,
                                                          IE_dummy,
                                                          Chrome_dummy,
                                                          Firefox_dummy,
                                                          Social_Media_Use)

#Remove NA values:
data_for_analysis <- Clean(data_for_analysis)

#Use glmnet lasso to choose covariates to be a part of model:
names_of_columns_3 <- Lasso(data_for_analysis)

names_of_columns_3 <- c('Treated',names_of_columns_3)

names.use <- names(data_frame_1)[(names(data_frame_1) %in% names_of_columns_3)]

data_for_analysis <- data_frame_1[, names.use]

data_for_analysis <- do.call(data.frame,                      # Replace Inf in data by NA
                             lapply(data_for_analysis,
                                    function(x) replace(x, is.infinite(x), NA)))

ivreg_media_trust_compl_2 <- lm_robust(Trust_Media_w2 ~ ., data = data_for_analysis)


################################# Hypothesis 2b: Trust in Reliable Source (CBS) ################################

data_for_analysis <- data_frame_1 %>% ungroup() %>% select(CBS_Trust_2,
                                                          CBS_Trust_1,
                                                          gender_dummy_fem,
                                                          educ_score,
                                                          Age,
                                                          Age_Sq,
                                                          party_score,
                                                          race_white,
                                                          ideo_score,
                                                          Trust_Media_w1,
                                                          trust_news,
                                                          trust_news_sm,
                                                          cons_news_n,
                                                          cons_cable,
                                                          cons_print,
                                                          cons_public, 
                                                          cons_talk,
                                                          cons_desk,
                                                          cons_mobile,
                                                          Safari_dummy,
                                                          IE_dummy,
                                                          Chrome_dummy,
                                                          Firefox_dummy,
                                                          Social_Media_Use)

#Remove NA values:
data_for_analysis <- Clean(data_for_analysis)

#Use glmnet lasso to choose covariates to be a part of model:
names_of_columns_3 <- Lasso(data_for_analysis)

names_of_columns_3 <- c('Treated',names_of_columns_3)

names.use <- names(data_frame_1)[(names(data_frame_1) %in% names_of_columns_3)]

data_for_analysis <- data_frame_1[, names.use]

data_for_analysis <- do.call(data.frame,                      # Replace Inf in data by NA
                             lapply(data_for_analysis,
                                    function(x) replace(x, is.infinite(x), NA)))

ivreg_CBS_compl_2 <- lm_robust(CBS_Trust_2 ~ ., data = data_for_analysis)

################################# Hypothesis 2c: Trust in Reliable Source (ABC) ################################


data_for_analysis <- data_frame_1 %>% ungroup() %>% select(ABC_Trust_2,
                                                          ABC_Trust_1,
                                                          gender_dummy_fem,
                                                          educ_score,
                                                          Age,
                                                          Age_Sq,
                                                          party_score,
                                                          race_white,
                                                          ideo_score,
                                                          Trust_Media_w1,
                                                          trust_news,
                                                          trust_news_sm,
                                                          cons_news_n,
                                                          cons_cable,
                                                          cons_print,
                                                          cons_public, 
                                                          cons_talk,
                                                          cons_desk,
                                                          cons_mobile,
                                                          Safari_dummy,
                                                          IE_dummy,
                                                          Chrome_dummy,
                                                          Firefox_dummy,
                                                          Social_Media_Use)

#Remove NA values:
data_for_analysis <- Clean(data_for_analysis)

#Use glmnet lasso to choose covariates to be a part of model:
names_of_columns_3 <- Lasso(data_for_analysis)

names_of_columns_3 <- c('Treated',names_of_columns_3)

names.use <- names(data_frame_1)[(names(data_frame_1) %in% names_of_columns_3)]

data_for_analysis <- data_frame_1[, names.use]

data_for_analysis <- do.call(data.frame,                      # Replace Inf in data by NA
                             lapply(data_for_analysis,
                                    function(x) replace(x, is.infinite(x), NA)))

ivreg_ABC_compl_2 <- lm_robust(ABC_Trust_2 ~ ., data = data_for_analysis)


################################# Hypothesis 2d: Trust in Reliable Source (NBC) ################################
data_for_analysis <- data_frame_1 %>% ungroup() %>% select(NBC_Trust_2,
                                                          NBC_Trust_1,
                                                          gender_dummy_fem,
                                                          educ_score,
                                                          Age,
                                                          Age_Sq,
                                                          party_score,
                                                          race_white,
                                                          ideo_score,
                                                          Trust_Media_w1,
                                                          trust_news,
                                                          trust_news_sm,
                                                          cons_news_n,
                                                          cons_cable,
                                                          cons_print,
                                                          cons_public, 
                                                          cons_talk,
                                                          cons_desk,
                                                          cons_mobile,
                                                          Safari_dummy,
                                                          IE_dummy,
                                                          Chrome_dummy,
                                                          Firefox_dummy,
                                                          Social_Media_Use)


#Remove NA values:
data_for_analysis <- Clean(data_for_analysis)

#Use glmnet lasso to choose covariates to be a part of model:
names_of_columns_3 <- Lasso(data_for_analysis)

names_of_columns_3 <- c('Treated',names_of_columns_3)

names.use <- names(data_frame_1)[(names(data_frame_1) %in% names_of_columns_3)]

data_for_analysis <- data_frame_1[, names.use]

data_for_analysis <- do.call(data.frame,                      # Replace Inf in data by NA
                             lapply(data_for_analysis,
                                    function(x) replace(x, is.infinite(x), NA)))

ivreg_NBC_compl_2 <- lm_robust(NBC_Trust_2 ~ ., data = data_for_analysis)

################################# Hypothesis 2e: Trust in Reliable Source (CNN) ################################

data_for_analysis <- data_frame_1 %>% ungroup() %>% select(CNN_Trust_2,
                                                          CNN_Trust_1,
                                                          gender_dummy_fem,
                                                          educ_score,
                                                          Age,
                                                          Age_Sq,
                                                          party_score,
                                                          race_white,
                                                          ideo_score,
                                                          Trust_Media_w1,
                                                          trust_news,
                                                          trust_news_sm,
                                                          cons_news_n,
                                                          cons_cable,
                                                          cons_print,
                                                          cons_public, 
                                                          cons_talk,
                                                          cons_desk,
                                                          cons_mobile,
                                                          Safari_dummy,
                                                          IE_dummy,
                                                          Chrome_dummy,
                                                          Firefox_dummy,
                                                          Social_Media_Use)


#Remove NA values:
data_for_analysis <- Clean(data_for_analysis)

#Use glmnet lasso to choose covariates to be a part of model:
names_of_columns_3 <- Lasso(data_for_analysis)

names_of_columns_3 <- c('Treated',names_of_columns_3)

names.use <- names(data_frame_1)[(names(data_frame_1) %in% names_of_columns_3)]

data_for_analysis <- data_frame_1[, names.use]

data_for_analysis <- do.call(data.frame,                      # Replace Inf in data by NA
                             lapply(data_for_analysis,
                                    function(x) replace(x, is.infinite(x), NA)))

ivreg_CNN_compl_2 <- lm_robust(CNN_Trust_2 ~ ., data = data_for_analysis)


################################# Hypothesis 2f: Trust in Reliable Source (Fox News) ################################


data_for_analysis <- data_frame_1 %>% ungroup() %>% select(Fox_Trust_2,
                                                          Fox_Trust_1,
                                                          gender_dummy_fem,
                                                          educ_score,
                                                          Age,
                                                          Age_Sq,
                                                          party_score,
                                                          race_white,
                                                          ideo_score,
                                                          Trust_Media_w1,
                                                          trust_news,
                                                          trust_news_sm,
                                                          cons_news_n,
                                                          cons_cable,
                                                          cons_print,
                                                          cons_public, 
                                                          cons_talk,
                                                          cons_desk,
                                                          cons_mobile,
                                                          Safari_dummy,
                                                          IE_dummy,
                                                          Chrome_dummy,
                                                          Firefox_dummy,
                                                          Social_Media_Use)


#Remove NA values:
data_for_analysis <- Clean(data_for_analysis)

#Use glmnet lasso to choose covariates to be a part of model:
names_of_columns_3 <- Lasso(data_for_analysis)

names_of_columns_3 <- c('Treated',names_of_columns_3)

names.use <- names(data_frame_1)[(names(data_frame_1) %in% names_of_columns_3)]

data_for_analysis <- data_frame_1[, names.use]

data_for_analysis <- do.call(data.frame,                      # Replace Inf in data by NA
                             lapply(data_for_analysis,
                                    function(x) replace(x, is.infinite(x), NA)))

ivreg_Fox_compl_2 <- lm_robust(Fox_Trust_2 ~ ., data = data_for_analysis)

################################# Hypothesis 3a: Affective Polarization ################################
data_for_analysis <- data_frame_1 %>% ungroup() %>% select(aff_pol_w2,
                                                          aff_pol_w1,
                                                          gender_dummy_fem,
                                                          educ_score,
                                                          Age,
                                                          Age_Sq,
                                                          party_score,
                                                          race_white,
                                                          ideo_score,
                                                          Trust_Media_w1,
                                                          trust_news,
                                                          trust_news_sm,
                                                          cons_news_n,
                                                          cons_cable,
                                                          cons_print,
                                                          cons_public, 
                                                          cons_talk,
                                                          cons_desk,
                                                          cons_mobile,
                                                          Safari_dummy,
                                                          IE_dummy,
                                                          Chrome_dummy,
                                                          Firefox_dummy,
                                                          Social_Media_Use)

#Remove NA values:
data_for_analysis <- Clean(data_for_analysis)

#Use glmnet lasso to choose covariates to be a part of model:
names_of_columns_3 <- Lasso(data_for_analysis)

names_of_columns_3 <- c('Treated',names_of_columns_3)

names.use <- names(data_frame_1)[(names(data_frame_1) %in% names_of_columns_3)]

data_for_analysis <- data_frame_1[, names.use]

data_for_analysis <- do.call(data.frame,                      # Replace Inf in data by NA
                             lapply(data_for_analysis,
                                    function(x) replace(x, is.infinite(x), NA)))

ivreg_aff_pol_compl_2 <- lm_robust(aff_pol_w2 ~ ., data = data_for_analysis)

################################# Hypothesis 3b: Political Cynicism ################################

data_for_analysis <- data_frame_1 %>% ungroup() %>% select(Pol_cyn_2,
                                                          Pol_cyn_1,
                                                          gender_dummy_fem,
                                                          educ_score,
                                                          Age,
                                                          Age_Sq,
                                                          party_score,
                                                          race_white,
                                                          ideo_score,
                                                          Trust_Media_w1,
                                                          trust_news,
                                                          trust_news_sm,
                                                          cons_news_n,
                                                          cons_cable,
                                                          cons_print,
                                                          cons_public, 
                                                          cons_talk,
                                                          cons_desk,
                                                          cons_mobile,
                                                          Safari_dummy,
                                                          IE_dummy,
                                                          Chrome_dummy,
                                                          Firefox_dummy,
                                                          Social_Media_Use)

#Remove NA values:
data_for_analysis <- Clean(data_for_analysis)

#Use glmnet lasso to choose covariates to be a part of model:
names_of_columns_3 <- Lasso(data_for_analysis)

names_of_columns_3 <- c('Treated',names_of_columns_3)

names.use <- names(data_frame_1)[(names(data_frame_1) %in% names_of_columns_3)]

data_for_analysis <- data_frame_1[, names.use]

data_for_analysis <- do.call(data.frame,                      # Replace Inf in data by NA
                             lapply(data_for_analysis,
                                    function(x) replace(x, is.infinite(x), NA)))

ivreg_Pol_cyn_compl_2 <- lm_robust(Pol_cyn_2 ~ ., data = data_for_analysis)

########################## Testing Research Question 1 and 2 ###########################


###### Research Question 1: Can it can reduce people's beliefs in both accurate and inaccurate information ######


################################# BLM Misinformation Index ################################

data_for_analysis <- data_frame_1 %>% ungroup() %>% select(BLM_Misinfo_Index_w2,
                                                          gender_dummy_fem,
                                                          educ_score,
                                                          Age,
                                                          Age_Sq,
                                                          party_score,
                                                          race_white,
                                                          ideo_score,
                                                          Trust_Media_w1,
                                                          trust_news,
                                                          trust_news_sm,
                                                          cons_news_n,
                                                          cons_cable,
                                                          cons_print,
                                                          cons_public, 
                                                          cons_talk,
                                                          cons_desk,
                                                          cons_mobile,
                                                          Safari_dummy,
                                                          IE_dummy,
                                                          Chrome_dummy,
                                                          Firefox_dummy,
                                                          Social_Media_Use)



#Remove NA values:
data_for_analysis <- Clean(data_for_analysis)

#Use glmnet lasso to choose covariates to be a part of model:
names_of_columns_3 <- Lasso(data_for_analysis)

names_of_columns_3 <- c('Treated',names_of_columns_3)

names.use <- names(data_frame_1)[(names(data_frame_1) %in% names_of_columns_3)]

data_for_analysis <- data_frame_1[, names.use]

data_for_analysis <- do.call(data.frame,                      # Replace Inf in data by NA
                             lapply(data_for_analysis,
                                    function(x) replace(x, is.infinite(x), NA)))

ivreg_BLM_Misinfo_compl_2 <- lm_robust(BLM_Misinfo_Index_w2 ~ ., data = data_for_analysis)

################################# Covid Misinformation Index ################################
data_for_analysis <- data_frame_1 %>% ungroup() %>% select(Covid_Misinfo_Index_w2,
                                                          gender_dummy_fem,
                                                          educ_score,
                                                          Age,
                                                          Age_Sq,
                                                          party_score,
                                                          race_white,
                                                          ideo_score,
                                                          Trust_Media_w1,
                                                          trust_news,
                                                          trust_news_sm,
                                                          cons_news_n,
                                                          cons_cable,
                                                          cons_print,
                                                          cons_public, 
                                                          cons_talk,
                                                          cons_desk,
                                                          cons_mobile,
                                                          Safari_dummy,
                                                          IE_dummy,
                                                          Chrome_dummy,
                                                          Firefox_dummy,
                                                          Social_Media_Use)




#Remove NA values:
data_for_analysis <- Clean(data_for_analysis)

#Use glmnet lasso to choose covariates to be a part of model:
names_of_columns_3 <- Lasso(data_for_analysis)

names_of_columns_3 <- c('Treated',names_of_columns_3)

names.use <- names(data_frame_1)[(names(data_frame_1) %in% names_of_columns_3)]

data_for_analysis <- data_frame_1[, names.use]

data_for_analysis <- do.call(data.frame,                      # Replace Inf in data by NA
                             lapply(data_for_analysis,
                                    function(x) replace(x, is.infinite(x), NA)))

ivreg_Covid_Misinfo_compl_2 <- lm_robust(Covid_Misinfo_Index_w2 ~ ., data = data_for_analysis)

################################# BLM Information Index ################################
data_for_analysis <- data_frame_1 %>% ungroup() %>% select(BLM_info_Index_w2,
                                                          gender_dummy_fem,
                                                          educ_score,
                                                          Age,
                                                          Age_Sq,
                                                          party_score,
                                                          race_white,
                                                          ideo_score,
                                                          Trust_Media_w1,
                                                          trust_news,
                                                          trust_news_sm,
                                                          cons_news_n,
                                                          cons_cable,
                                                          cons_print,
                                                          cons_public, 
                                                          cons_talk,
                                                          cons_desk,
                                                          cons_mobile,
                                                          Safari_dummy,
                                                          IE_dummy,
                                                          Chrome_dummy,
                                                          Firefox_dummy,
                                                          Social_Media_Use)

#Remove NA values:
data_for_analysis <- Clean(data_for_analysis)

#Use glmnet lasso to choose covariates to be a part of model:
names_of_columns_3 <- Lasso(data_for_analysis)

names_of_columns_3 <- c('Treated',names_of_columns_3)

names.use <- names(data_frame_1)[(names(data_frame_1) %in% names_of_columns_3)]

data_for_analysis <- data_frame_1[, names.use]

data_for_analysis <- do.call(data.frame,                      # Replace Inf in data by NA
                             lapply(data_for_analysis,
                                    function(x) replace(x, is.infinite(x), NA)))

ivreg_BLM_info_compl_2 <- lm_robust(BLM_info_Index_w2 ~ ., data = data_for_analysis)


################################# Covid-19 Information Index ################################
data_for_analysis <- data_frame_1 %>% ungroup() %>% select(Covid_info_Index_w2,
                                                          gender_dummy_fem,
                                                          educ_score,
                                                          Age,
                                                          Age_Sq,
                                                          party_score,
                                                          race_white,
                                                          ideo_score,
                                                          Trust_Media_w1,
                                                          trust_news,
                                                          trust_news_sm,
                                                          cons_news_n,
                                                          cons_cable,
                                                          cons_print,
                                                          cons_public, 
                                                          cons_talk,
                                                          cons_desk,
                                                          cons_mobile,
                                                          Safari_dummy,
                                                          IE_dummy,
                                                          Chrome_dummy,
                                                          Firefox_dummy,
                                                          Social_Media_Use)


#Remove NA values:
data_for_analysis <- Clean(data_for_analysis)

#Use glmnet lasso to choose covariates to be a part of model:
names_of_columns_3 <- Lasso(data_for_analysis)

names_of_columns_3 <- c('Treated',names_of_columns_3)

names.use <- names(data_frame_1)[(names(data_frame_1) %in% names_of_columns_3)]

data_for_analysis <- data_frame_1[, names.use]

data_for_analysis <- do.call(data.frame,                      # Replace Inf in data by NA
                             lapply(data_for_analysis,
                                    function(x) replace(x, is.infinite(x), NA)))

ivreg_Covid_info_compl_2 <- lm_robust(Covid_info_Index_w2 ~ ., data = data_for_analysis)

###### Research Question 2: We explore whether downstream effects occur on other outcomes such as trust in institutions, belief that ``fake news'' is a problem in general, and belief that ``fake news'' is a problem in the mainstream media ######

################################# Is Fake News a Problem in the Mainstream Media?  ################################
data_for_analysis <- data_frame_1 %>% ungroup() %>% select(SMP4310_w2,
                                                          SMP4310,
                                                          gender_dummy_fem,
                                                          educ_score,
                                                          Age,
                                                          Age_Sq,
                                                          party_score,
                                                          race_white,
                                                          ideo_score,
                                                          Trust_Media_w1,
                                                          trust_news,
                                                          trust_news_sm,
                                                          cons_news_n,
                                                          cons_cable,
                                                          cons_print,
                                                          cons_public, 
                                                          cons_talk,
                                                          cons_desk,
                                                          cons_mobile,
                                                          Safari_dummy,
                                                          IE_dummy,
                                                          Chrome_dummy,
                                                          Firefox_dummy,
                                                          Social_Media_Use)

#Remove NA values:
data_for_analysis <- Clean(data_for_analysis)

#Use glmnet lasso to choose covariates to be a part of model:
names_of_columns_3 <- Lasso(data_for_analysis)

names_of_columns_3 <- c('Treated',names_of_columns_3)

names.use <- names(data_frame_1)[(names(data_frame_1) %in% names_of_columns_3)]

data_for_analysis <- data_frame_1[, names.use]

data_for_analysis <- do.call(data.frame,                      # Replace Inf in data by NA
                             lapply(data_for_analysis,
                                    function(x) replace(x, is.infinite(x), NA)))

ivreg_FN_prob_main_compl_2 <- lm_robust(SMP4310_w2 ~ ., data = data_for_analysis)


################################# Fake News is a Problem  ################################

data_for_analysis <- data_frame_1 %>% ungroup() %>% select(SMP4326_w2,
                                                          SMP4326,
                                                          gender_dummy_fem,
                                                          educ_score,
                                                          Age,
                                                          Age_Sq,
                                                          party_score,
                                                          race_white,
                                                          ideo_score,
                                                          Trust_Media_w1,
                                                          trust_news,
                                                          trust_news_sm,
                                                          cons_news_n,
                                                          cons_cable,
                                                          cons_print,
                                                          cons_public, 
                                                          cons_talk,
                                                          cons_desk,
                                                          cons_mobile,
                                                          Safari_dummy,
                                                          IE_dummy,
                                                          Chrome_dummy,
                                                          Firefox_dummy,
                                                          Social_Media_Use)

#Remove NA values:
data_for_analysis <- Clean(data_for_analysis)

#Use glmnet lasso to choose covariates to be a part of model:
names_of_columns_3 <- Lasso(data_for_analysis)

names_of_columns_3 <- c('Treated',names_of_columns_3)

names.use <- names(data_frame_1)[(names(data_frame_1) %in% names_of_columns_3)]

data_for_analysis <- data_frame_1[, names.use]

data_for_analysis <- do.call(data.frame,                      # Replace Inf in data by NA
                             lapply(data_for_analysis,
                                    function(x) replace(x, is.infinite(x), NA)))


ivreg_FN_prob_compl_2 <- lm_robust(SMP4326_w2 ~ ., data = data_for_analysis)

################################# Hypothesis: Trust in Institutions  ################################

data_for_analysis <- data_frame_1 %>% ungroup() %>% select(Trust_inst_w2,
                                                          Trust_inst_w1,
                                                          gender_dummy_fem,
                                                          educ_score,
                                                          Age,
                                                          Age_Sq,
                                                          party_score,
                                                          race_white,
                                                          ideo_score,
                                                          Trust_Media_w1,
                                                          trust_news,
                                                          trust_news_sm,
                                                          cons_news_n,
                                                          cons_cable,
                                                          cons_print,
                                                          cons_public, 
                                                          cons_talk,
                                                          cons_desk,
                                                          cons_mobile,
                                                          Safari_dummy,
                                                          IE_dummy,
                                                          Chrome_dummy,
                                                          Firefox_dummy,
                                                          Social_Media_Use)

#Remove NA values:
data_for_analysis <- Clean(data_for_analysis)

#Use glmnet lasso to choose covariates to be a part of model:
names_of_columns_3 <- Lasso(data_for_analysis)


names_of_columns_3 <- c('Treated',names_of_columns_3)

names.use <- names(data_frame_1)[(names(data_frame_1) %in% names_of_columns_3)]

data_for_analysis <- data_frame_1[, names.use]

data_for_analysis <- do.call(data.frame,                      # Replace Inf in data by NA
                             lapply(data_for_analysis,
                                    function(x) replace(x, is.infinite(x), NA)))


ivreg_Trust_inst_compl_2 <- lm_robust(Trust_inst_w2 ~ ., data = data_for_analysis)




#Figure 2 in Paper:

Period <- c('Before July 1st',
            'July 1st - July 13th',
            'Before July 1st',
            'July 1st - July 13th',
            'Before July 1st',
            'July 1st - July 13th',
            'Before July 1st',
            'July 1st - July 13th',
            'Before July 1st',
            'July 1st - July 13th')



Coef_names <- c('Proportion of Online News Consumed that is Unreliable',
                'Proportion of Online News Consumed that is Unreliable',
                'Count of Online News Consumed that is Unreliable',
                'Count of Online News Consumed that is Unreliable',
                'Proportion of Online News Consumed that is Reliable',
                'Proportion of Online News Consumed that is Reliable',
                'Count of Online News Consumed that is Reliable',
                'Count of Online News Consumed that is Reliable',
                'Average Reliability Score of Online News Consumed',
                'Average Reliability Score of Online News Consumed')



Coefficients <- c(ivreg_Unrel_dv_compl_2$coefficients[2]/sd(Pulse_data$Prop_Unreliable_NewsG_Score,na.rm=T),
                  ivreg_Unrel_Post_compl_2$coefficients[2]/sd(Pulse_data$Prop_Unreliable_NewsG_Score,na.rm=T),
                  ivreg_Unrel_c_dv_compl_2$coefficients[2]/sd(Pulse_data$Count_Unreliable_NewsG_Score,na.rm=T),
                  ivreg_Unrel_c_Post_compl_2$coefficients[2]/sd(Pulse_data$Count_Unreliable_NewsG_Score,na.rm=T),
                  ivreg_Rel_dv_compl_2$coefficients[2]/sd(Pulse_data$Prop_Reliable_NewsG_Score,na.rm=T),
                  ivreg_Rel_Post_compl_2$coefficients[2]/sd(Pulse_data$Prop_Reliable_NewsG_Score,na.rm=T),
                  ivreg_rel_c_dv_compl_2$coefficients[2]/sd(Pulse_data$Count_Reliable_NewsG_Score,na.rm=T),
                  ivreg_rel_c_Post_compl_2$coefficients[2]/sd(Pulse_data$Count_Reliable_NewsG_Score,na.rm=T),
                  ivreg_news_dv_compl_2$coefficients[2]/sd(Pulse_data$Average_domain_NewsG_Score,na.rm=T),
                  ivreg_news_Post_compl_2$coefficients[2]/sd(Pulse_data$Average_domain_NewsG_Score,na.rm=T))                 


CI_Upper <- c(ivreg_Unrel_dv_compl_2$conf.high[2]/sd(Pulse_data$Prop_Unreliable_NewsG_Score,na.rm=T),
              ivreg_Unrel_Post_compl_2$conf.high[2]/sd(Pulse_data$Prop_Unreliable_NewsG_Score,na.rm=T),
              ivreg_Unrel_c_dv_compl_2$conf.high[2]/sd(Pulse_data$Count_Unreliable_NewsG_Score,na.rm=T),
              ivreg_Unrel_c_Post_compl_2$conf.high[2]/sd(Pulse_data$Count_Unreliable_NewsG_Score,na.rm=T),
              ivreg_Rel_dv_compl_2$conf.high[2]/sd(Pulse_data$Prop_Reliable_NewsG_Score,na.rm=T),
              ivreg_Rel_Post_compl_2$conf.high[2]/sd(Pulse_data$Prop_Reliable_NewsG_Score,na.rm=T),
              ivreg_rel_c_dv_compl_2$conf.high[2]/sd(Pulse_data$Count_Reliable_NewsG_Score,na.rm=T),
              ivreg_rel_c_Post_compl_2$conf.high[2]/sd(Pulse_data$Count_Reliable_NewsG_Score,na.rm=T),
              ivreg_news_dv_compl_2$conf.high[2]/sd(Pulse_data$Average_domain_NewsG_Score,na.rm=T),
              ivreg_news_Post_compl_2$conf.high[2]/sd(Pulse_data$Average_domain_NewsG_Score,na.rm=T))                 



CI_Lower <- c(ivreg_Unrel_dv_compl_2$conf.low[2]/sd(Pulse_data$Prop_Unreliable_NewsG_Score,na.rm=T),
              ivreg_Unrel_Post_compl_2$conf.low[2]/sd(Pulse_data$Prop_Unreliable_NewsG_Score,na.rm=T),
              ivreg_Unrel_c_dv_compl_2$conf.low[2]/sd(Pulse_data$Count_Unreliable_NewsG_Score,na.rm=T),
              ivreg_Unrel_c_Post_compl_2$conf.low[2]/sd(Pulse_data$Count_Unreliable_NewsG_Score,na.rm=T),
              ivreg_Rel_dv_compl_2$conf.low[2]/sd(Pulse_data$Prop_Reliable_NewsG_Score,na.rm=T),
              ivreg_Rel_Post_compl_2$conf.low[2]/sd(Pulse_data$Prop_Reliable_NewsG_Score,na.rm=T),
              ivreg_rel_c_dv_compl_2$conf.low[2]/sd(Pulse_data$Count_Reliable_NewsG_Score,na.rm=T),
              ivreg_rel_c_Post_compl_2$conf.low[2]/sd(Pulse_data$Count_Reliable_NewsG_Score,na.rm=T),
              ivreg_news_dv_compl_2$conf.low[2]/sd(Pulse_data$Average_domain_NewsG_Score,na.rm=T),
              ivreg_news_Post_compl_2$conf.low[2]/sd(Pulse_data$Average_domain_NewsG_Score,na.rm=T))                 




d_matrix <- cbind(Period,Coef_names,Coefficients,CI_Upper,CI_Lower)
rownames(d_matrix) <- c()

d_matrix <- data.frame(d_matrix)


d_matrix$Coefficients <- as.character(d_matrix$Coefficients)
d_matrix$CI_Lower <- as.character(d_matrix$CI_Lower)
d_matrix$CI_Upper <- as.character(d_matrix$CI_Upper)

d_matrix$Coefficients <- as.numeric(d_matrix$Coefficients)
d_matrix$CI_Lower <- as.numeric(d_matrix$CI_Lower)
d_matrix$CI_Upper <- as.numeric(d_matrix$CI_Upper)


d_matrix <- d_matrix %>% arrange(desc(row_number()))




d_matrix$x<-c(0.8,0.9,1.3,1.4,1.8,1.9,2.3,2.4,2.8,2.9)

ggplot(data = d_matrix, aes(x = x, y = Coefficients)) +
  geom_hline(aes(yintercept = 0), color = "gray",
             linetype = 2, size = 1.2) +
  geom_point(aes(color = Period, shape=Period),size=4) +
  geom_linerange(aes(min = CI_Lower, 
                     max = CI_Upper, 
                     color = Period),
                 size=1.5) +
  scale_color_manual(values=c('red','blue','purple'), name = "Period") +
  ylab("\n Effect of NewsGuard Intervention on Online Behavioral Measures         \n(1 unit is 1 standard deviation of that measure pre-treatment)         ") +
  theme_classic() +
  theme(axis.title.x = element_text(size=16),
        axis.text.x  = element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.text.y  = element_text(size=16),
        plot.title = element_text(size = 16),
        legend.title = element_text(size=16),
        legend.text = element_text(size=14)) +
  ylim(-0.3,0.3) +
  scale_x_continuous(" \n",breaks=c(2.85,2.35,1.85,1.35,0.85),labels=c('Proportion of Online News\nConsumed that is Unreliable',
                                                                       'Count of Online News\nConsumed that is Unreliable',
                                                                       'Proportion of Online News\nConsumed that is Reliable',
                                                                       'Count of Online News\nConsumed that is Reliable',
                                                                       'Average Reliability Score\nof Online News Consumed'),limits=c(0.5,3.4)) +
  coord_flip()

ggsave('./Figures/Behavioral_Coefficients.png',height=12,width=10)

#Figure 4 in Paper:
Coef_names <- c('Trust in Media',
                'Trust in CBS',
                'Trust in ABC',
                'Trust in NBC',
                'Trust in CNN',
                'Trust in Fox News',
                'Affective Polarization',
                'Political Cynicism',
                'Belief in BLM Misinformation',
                'Belief in Covid Misinformation',
                'Belief in BLM True information',
                'Belief in Covid True information',
                'Trust in Institutions',
                'Belief that \"fake news is \n a problem\"',
                'Belief that \"fake news is \n a problem in the mainstream media\"')

Coefficients <- c(ivreg_media_trust_compl_2$coefficients[2]/sd(data_frame_1$Trust_Media_w1,na.rm=T),
                  ivreg_CBS_compl_2$coefficients[2]/sd(data_frame_1$CBS_Trust_1,na.rm=T),
                  ivreg_ABC_compl_2$coefficients[2]/sd(data_frame_1$ABC_Trust_1,na.rm=T),
                  ivreg_NBC_compl_2$coefficients[2]/sd(data_frame_1$NBC_Trust_1,na.rm=T),
                  ivreg_CNN_compl_2$coefficients[2]/sd(data_frame_1$CNN_Trust_1,na.rm=T),
                  ivreg_Fox_compl_2$coefficients[2]/sd(data_frame_1$Fox_Trust_1,na.rm=T),
                  ivreg_aff_pol_compl_2$coefficients[2]/sd(data_frame_1$aff_pol_w1,na.rm=T),
                  ivreg_Pol_cyn_compl_2$coefficients[2]/sd(data_frame_1$Pol_cyn_1,na.rm=T),
                  ivreg_BLM_Misinfo_compl_2$coefficients[2]/sd(data_frame_1$BLM_Misinfo_Index_w2,na.rm=T),
                  ivreg_Covid_Misinfo_compl_2$coefficients[2]/sd(data_frame_1$Covid_Misinfo_Index_w2,na.rm=T),
                  ivreg_BLM_info_compl_2$coefficients[2]/sd(data_frame_1$BLM_info_Index_w2,na.rm=T),
                  ivreg_Covid_info_compl_2$coefficients[2]/sd(data_frame_1$Covid_info_Index_w2,na.rm=T),
                  ivreg_Trust_inst_compl_2$coefficients[2]/sd(data_frame_1$Trust_inst_w1,na.rm=T),
                  ivreg_FN_prob_compl_2$coefficients[3]/sd(data_frame_1$SMP4326,na.rm=T),
                  ivreg_FN_prob_main_compl_2$coefficients[3]/sd(data_frame_1$SMP4310,na.rm=T))


CI_Upper <- c(ivreg_media_trust_compl_2$conf.high[2]/sd(data_frame_1$Trust_Media_w1,na.rm=T),
              ivreg_CBS_compl_2$conf.high[2]/sd(data_frame_1$CBS_Trust_1,na.rm=T),
              ivreg_ABC_compl_2$conf.high[2]/sd(data_frame_1$ABC_Trust_1,na.rm=T),
              ivreg_NBC_compl_2$conf.high[2]/sd(data_frame_1$NBC_Trust_1,na.rm=T),
              ivreg_CNN_compl_2$conf.high[2]/sd(data_frame_1$CNN_Trust_1,na.rm=T),
              ivreg_Fox_compl_2$conf.high[2]/sd(data_frame_1$Fox_Trust_1,na.rm=T),
              ivreg_aff_pol_compl_2$conf.high[2]/sd(data_frame_1$aff_pol_w1,na.rm=T),
              ivreg_Pol_cyn_compl_2$conf.high[2]/sd(data_frame_1$Pol_cyn_1,na.rm=T),
              ivreg_BLM_Misinfo_compl_2$conf.high[2]/sd(data_frame_1$BLM_Misinfo_Index_w2,na.rm=T),
              ivreg_Covid_Misinfo_compl_2$conf.high[2]/sd(data_frame_1$Covid_Misinfo_Index_w2,na.rm=T),
              ivreg_BLM_info_compl_2$conf.high[2]/sd(data_frame_1$BLM_info_Index_w2,na.rm=T),
              ivreg_Covid_info_compl_2$conf.high[2]/sd(data_frame_1$Covid_info_Index_w2,na.rm=T),
              ivreg_Trust_inst_compl_2$conf.high[2]/sd(data_frame_1$Trust_inst_w1,na.rm=T),
              ivreg_FN_prob_compl_2$conf.high[3]/sd(data_frame_1$SMP4326,na.rm=T),
              ivreg_FN_prob_main_compl_2$conf.high[3]/sd(data_frame_1$SMP4310,na.rm=T))              

CI_Lower <- c(ivreg_media_trust_compl_2$conf.low[2]/sd(data_frame_1$Trust_Media_w1,na.rm=T),
              ivreg_CBS_compl_2$conf.low[2]/sd(data_frame_1$CBS_Trust_1,na.rm=T),
              ivreg_ABC_compl_2$conf.low[2]/sd(data_frame_1$ABC_Trust_1,na.rm=T),
              ivreg_NBC_compl_2$conf.low[2]/sd(data_frame_1$NBC_Trust_1,na.rm=T),
              ivreg_CNN_compl_2$conf.low[2]/sd(data_frame_1$CNN_Trust_1,na.rm=T),
              ivreg_Fox_compl_2$conf.low[2]/sd(data_frame_1$Fox_Trust_1,na.rm=T),
              ivreg_aff_pol_compl_2$conf.low[2]/sd(data_frame_1$aff_pol_w1,na.rm=T),
              ivreg_Pol_cyn_compl_2$conf.low[2]/sd(data_frame_1$Pol_cyn_1,na.rm=T),
              ivreg_BLM_Misinfo_compl_2$conf.low[2]/sd(data_frame_1$BLM_Misinfo_Index_w2,na.rm=T),
              ivreg_Covid_Misinfo_compl_2$conf.low[2]/sd(data_frame_1$Covid_Misinfo_Index_w2,na.rm=T),
              ivreg_BLM_info_compl_2$conf.low[2]/sd(data_frame_1$BLM_info_Index_w2,na.rm=T),
              ivreg_Covid_info_compl_2$conf.low[2]/sd(data_frame_1$Covid_info_Index_w2,na.rm=T),
              ivreg_Trust_inst_compl_2$conf.low[2]/sd(data_frame_1$Trust_inst_w1,na.rm=T),
              ivreg_FN_prob_compl_2$conf.low[3]/sd(data_frame_1$SMP4326,na.rm=T),
              ivreg_FN_prob_main_compl_2$conf.low[3]/sd(data_frame_1$SMP4310,na.rm=T))                



d_matrix <- cbind(Coef_names,Coefficients,CI_Upper,CI_Lower)
rownames(d_matrix) <- c()

d_matrix <- data.frame(d_matrix)


d_matrix$Coefficients <- as.character(d_matrix$Coefficients)
d_matrix$CI_Lower <- as.character(d_matrix$CI_Lower)
d_matrix$CI_Upper <- as.character(d_matrix$CI_Upper)

d_matrix$Coefficients <- as.numeric(d_matrix$Coefficients)
d_matrix$CI_Lower <- as.numeric(d_matrix$CI_Lower)
d_matrix$CI_Upper <- as.numeric(d_matrix$CI_Upper)


d_matrix <- d_matrix %>% arrange(desc(row_number()))




d_matrix$x<-c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0,1.1,1.2,1.3,1.4,1.5)

ggplot(data = d_matrix, aes(x = x, y = Coefficients)) +
  geom_hline(aes(yintercept = 0), color = "gray",
             linetype = 2, size = 1.2) +
  geom_point(size=4) +
  geom_linerange(aes(min = CI_Lower, 
                     max = CI_Upper),
                 size=1.5) +
  scale_color_manual(values=c('red','blue','purple'), name = "Period") +
  ylab("\nEffect of NewsGuard Intervention on Perceptions      \n(1 unit is 1 standard deviation of that measure pre-treatment)       ") +
  theme_classic() +
  theme(axis.title.x = element_text(size=16),
        axis.text.x  = element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.text.y  = element_text(size=16),
        plot.title = element_text(size = 16),
        legend.title = element_text(size=16),
        legend.text = element_text(size=14)) +
  ylim(-0.3,0.3) +
  scale_x_continuous(" \n",breaks=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0,1.1,1.2,1.3,1.4,1.5),labels=rev(c('Trust in Media',
                                                                                                              'Trust in CBS',
                                                                                                              'Trust in ABC',
                                                                                                              'Trust in NBC',
                                                                                                              'Trust in CNN',
                                                                                                              'Trust in Fox News',
                                                                                                              'Affective Polarization',
                                                                                                              'Political Cynicism',
                                                                                                              'Belief in BLM Misinformation',
                                                                                                              'Belief in Covid Misinformation',
                                                                                                              'Belief in BLM True information',
                                                                                                              'Belief in Covid True information',
                                                                                                              'Trust in Institutions',
                                                                                                              'Belief that \"fake news is \n a problem\"',
                                                                                                              'Belief that \"fake news is \n a problem in the mainstream media\"')),limits=c(0.0,1.6)) + 
  coord_flip()


ggsave('./Figures/Survey_Coefficients.png',height=12,width=10)






#Read in survey data with digital trace data:
Pulse_data <- read_csv('./Data/Clean_NewsGuard_Digital_Trace_Data.csv',
                       col_types = cols(
                         .default= col_character(),
                         Treated = col_double(),
                         Complied=col_double(),
                         Total_DL =  col_double(),
                         Inverse_DL = col_double(),
                         Total_Science_Misinfo  =  col_double(),
                         Social_Media_Use =  col_double(),
                         income_score =  col_double(),
                         abs_part =  col_double(),
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
                         log_news=col_double()
                       ))



#Read in survey data without digital trace data:
#Table 1: Produce Descriptive statistics for sample by treatment and control groups:

Summary_stats <- Pulse_data %>% mutate(Groups = ifelse(Treated == 1 & !is.na(Pulse_Dummy),'Treated','No Pulse Data'))
Summary_stats <- Summary_stats %>% mutate(Groups = ifelse(Treated == 0 & !is.na(Pulse_Dummy),'Control',Groups))
Summary_stats <- Summary_stats %>% filter(Groups != 'No Pulse Data')

Summary_stats_1 <- Summary_stats %>% group_by(Groups) %>% mutate(grp.mean = mean(Average_domain_NewsG_Score,na.rm=T))

Summary_stats_1 <- Summary_stats_1 %>% select(Groups,grp.mean)

Summary_stats_1 <- unique(Summary_stats_1)

Summary_stats <- Summary_stats %>% select(Groups,Average_domain_NewsG_Score)


# Use semi-transparent fill
ggplot(Summary_stats, aes(x=Average_domain_NewsG_Score, fill=Groups,color=Groups)) +
  geom_density(alpha=0.4) +
  geom_vline(data=Summary_stats_1, aes(xintercept=grp.mean, color=Groups),
             linetype="dashed",size=1,alpha=0.8) +
  coord_cartesian(xlim=c(29,100),
                  ylim=c(0,0.14)) +
  xlab('\nAverage Reliability Score of Online News Consumed') + 
  ylab('Density\n') + 
  theme_classic() +
  theme(axis.title.x = element_text(size=16),
        axis.text.x  = element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.text.y  = element_text(size=16),
        plot.title = element_text(size = 16),
        legend.title = element_text(size=16),
        legend.text = element_text(size=14))

ggsave('./Figures/Distribution_1.png',height=5,width=10)



Summary_stats <- Pulse_data %>% mutate(Groups = ifelse(Treated == 1 & !is.na(Pulse_Dummy),'Treated','No Pulse Data'))
Summary_stats <- Summary_stats %>% mutate(Groups = ifelse(Treated == 0 & !is.na(Pulse_Dummy),'Control',Groups))
Summary_stats <- Summary_stats %>% filter(Groups != 'No Pulse Data')

Summary_stats_1 <- Summary_stats %>% group_by(Groups) %>% mutate(grp.mean = mean(Average_domain_NewsG_Score_dv,na.rm=T))

Summary_stats_1 <- Summary_stats_1 %>% select(Groups,grp.mean)

Summary_stats_1 <- unique(Summary_stats_1)

Summary_stats <- Summary_stats %>% select(Groups,Average_domain_NewsG_Score_dv)


# Use semi-transparent fill
ggplot(Summary_stats, aes(x=Average_domain_NewsG_Score_dv, fill=Groups,color=Groups)) +
  geom_density(alpha=0.4) +
  geom_vline(data=Summary_stats_1, aes(xintercept=grp.mean, color=Groups),
             linetype="dashed",size=1,alpha=0.8) +
  coord_cartesian(xlim=c(29,100),
                  ylim=c(0,0.14)) +
  xlab('\nAverage Reliability Score of Online News Consumed') + 
  ylab('Density\n') + 
  theme_classic() +
  theme(axis.title.x = element_text(size=16),
        axis.text.x  = element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.text.y  = element_text(size=16),
        plot.title = element_text(size = 16),
        legend.title = element_text(size=16),
        legend.text = element_text(size=14))

ggsave('./Figures/Distribution_2.png',height=5,width=10)





Summary_stats <- Pulse_data %>% mutate(Groups = ifelse(Treated == 1 & !is.na(Pulse_Dummy),'Treated','No Pulse Data'))
Summary_stats <- Summary_stats %>% mutate(Groups = ifelse(Treated == 0 & !is.na(Pulse_Dummy),'Control',Groups))
Summary_stats <- Summary_stats %>% filter(Groups != 'No Pulse Data')

Summary_stats_1 <- Summary_stats %>% group_by(Groups) %>% mutate(grp.mean = mean(Average_domain_NewsG_Score_post,na.rm=T))

Summary_stats_1 <- Summary_stats_1 %>% select(Groups,grp.mean)

Summary_stats_1 <- unique(Summary_stats_1)

Summary_stats <- Summary_stats %>% select(Groups,Average_domain_NewsG_Score_post)


# Use semi-transparent fill
ggplot(Summary_stats, aes(x=Average_domain_NewsG_Score_post, fill=Groups,color=Groups)) +
  geom_density(alpha=0.4) +
  geom_vline(data=Summary_stats_1, aes(xintercept=grp.mean, color=Groups),
             linetype="dashed",size=1,alpha=0.8) +
  xlab('\nAverage Reliability Score of Online News Consumed') + 
  ylab('Density\n') + 
  coord_cartesian(xlim=c(29,100),
                  ylim=c(0,0.14)) +
  theme_classic() +
  theme(axis.title.x = element_text(size=16),
        axis.text.x  = element_text(size=16),
        axis.title.y = element_text(size=16),
        axis.text.y  = element_text(size=16),
        plot.title = element_text(size = 16),
        legend.title = element_text(size=16),
        legend.text = element_text(size=14))

ggsave('./Figures/Distribution_3.png',height=5,width=10)



