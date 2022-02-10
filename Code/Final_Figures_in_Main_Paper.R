#################### Final_Figures_in_Main_Paper.R #############################
### Authors: Kevin Aslett, Andrew Guess, Jonathan Nagler, Richard Bonneau, and Joshua A. Tucker
### Purpose of code: Produce figures and tables of analyses that are displayed in the main text of the paper

##### Data In:
# (1) ./Data/Clean_NewsGuard_Digital_Trace_Data.csv
# (2) ./Data/Clean_NewsGuard_Survey_Study.csv
# (3) './Data/NYUU0017_w1_OUTPUT.csv'
# (4) './Data/NYUU0017_w2_OUTPUT.csv'
# (5) ./Data/NewsGuard_Ratings.csv
# (6) ./Data/pulsebyday.csv

##### Data Out: NONE

##### Figures Out:
# (1) Figure 4: Behavioral_Coefficients.png
# (3) Figure 3a: Distribution_1.png
# (4) Figure 3b: Distribution_2.png
# (5) Figure 3c: Distribution_3.png
# (6) Figure 5: Decile_Figure.png


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



#Figure 3A, 3B, and 3C: Create distributions of NewsGuard data:

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

#Create Treatment and Control Group Data:
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

### Test Hypotheses 1:
#(H1) We test whether in-feed source reliability labels shift downstream news and information consumption from unreliable sources known for publishing misleading or false content to more reliable sources

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


#Produce Figure 4 in Main Text:

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

#Create order of variables on y-axis:
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

#Save Figure 4 in the main text:
ggsave('./Figures/Behavioral_Coefficients.png',height=10,width=12)




############## Produce Figure 5 in the main text ################

################## Behavioral Measures (Covariate-Adjusted Models) ##############################################

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
                         compliance_check_1=col_double(),
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
list_variables_to_run_1 = list(c('Prop_Unreliable_NewsG_Score_dv','Prop_Unreliable_NewsG_Score'),
                               c('Prop_Reliable_NewsG_Score_dv','Prop_Reliable_NewsG_Score'),
                               c('Count_Unreliable_NewsG_Score_dv','Count_Unreliable_NewsG_Score'),
                               c('Count_Reliable_NewsG_Score_dv','Count_Reliable_NewsG_Score'),
                               c('Average_domain_NewsG_Score_dv','Average_domain_NewsG_Score'),
                               c('Prop_Unreliable_NewsG_Score_post','Prop_Unreliable_NewsG_Score'),
                               c('Prop_Reliable_NewsG_Score_post','Prop_Reliable_NewsG_Score'),
                               c('Count_Unreliable_NewsG_Score_post','Count_Unreliable_NewsG_Score'),
                               c('Count_Reliable_NewsG_Score_post','Count_Reliable_NewsG_Score'),
                               c('Average_domain_NewsG_Score_post','Average_domain_NewsG_Score'))

#Create list of Titles:
Titles <- c('Testing the Effect of the Intervention on Proportion of News Diet That is Unreliable with Covariate-Adjusted Models (HC2 Robust standard errors) (Before July 1st)',
            'Testing the Effect of the Intervention on Proportion of News Diet That is Reliable with Covariate-Adjusted Models (HC2 Robust standard errors) (Before July 1st)',
            'Testing the Effect of the Intervention on Count of Unreliable News Consumed with Covariate-Adjusted Models (HC2 Robust standard errors) (Before July 1st)',
            'Testing the Effect of the Intervention on Count of Reliable News Consumed with Covariate-Adjusted Models (HC2 Robust standard errors) (Before July 1st)',
            'Testing the Effect of the Intervention on Reliability Score of News Diet with Covariate-Adjusted Models (HC2 Robust standard errors) (Before July 1st)',
            'Testing the Effect of the Intervention on Proportion of News Diet That is Unreliable with Covariate-Adjusted Models (HC2 Robust standard errors) (After July 1st)',
            'Testing the Effect of the Intervention on Proportion of News Diet That is Reliable with Covariate-Adjusted Models (HC2 Robust standard errors) (After July 1st)',
            'Testing the Effect of the Intervention on Count of Unreliable News Consumed with Covariate-Adjusted Models (HC2 Robust standard errors) (After July 1st)',
            'Testing the Effect of the Intervention on Count of Reliable News Consumed with Covariate-Adjusted Models (HC2 Robust standard errors) (After July 1st)',
            'Testing the Effect of the Intervention on Reliability Score of News Diet with Covariate-Adjusted Models (HC2 Robust standard errors) (After July 1st)')

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


Decile_Names <- c('Bottom 10 Percent',
                  'Bottom 20 Percent',
                  'Bottom 30 Percent',
                  'Bottom 40 Percent',
                  'Bottom 50 Percent',
                  'Bottom 60 Percent',
                  'Bottom 70 Percent',
                  'Bottom 80 Percent',
                  'Bottom 90 Percent',
                  'Whole Sample')

Deciles <- quantile(Pulse_data$Average_domain_NewsG_Score, prob = c(0.1,0.2,0.3,0.4,0.5,0.60,0.70,0.80,0.90,1.0),na.rm=T)


i=1
x=1

#i=5
#i=10
for(i in 1:length(list_variables_to_run_1)){
  list_possible_covariates_for_use <- c(list_variables_to_run_1[[i]],list_possible_covariates)
  data_for_analysis <- Pulse_data %>% ungroup() %>% select(`list_possible_covariates_for_use`)
  
  #Clean Data:
  data_for_analysis <- Clean(data_for_analysis)
  
  #Use glmnet lasso to choose covariates to be a part of model:
  names_of_columns <- Lasso(data_for_analysis)
  
  #names_of_columns
  names_of_columns <- c('Treated',names_of_columns)
  
  new_matr <- matrix(ncol=6)
  if(i == 5){
    ci_matr <- matrix(ncol=4)
  }
  if(i == 10){
    ci_matr_2 <- matrix(ncol=4)
  }
  #x=1
  #x=2
  for(x in 1:length(Deciles)){
    #Pulse_data_2 <- Pulse_data %>% filter(Prop_Unreliable_NewsG_Score >= Deciles[x])
    Pulse_data_2 <- Pulse_data %>% filter(Average_domain_NewsG_Score <= Deciles[x])
    
    names_use <- names(Pulse_data_2)[(names(Pulse_data_2) %in% names_of_columns)]
    
    data_for_analysis <- Pulse_data_2[, names_use]
    data_for_analysis <- Clean(data_for_analysis)
    
    f <- paste0(list_variables_to_run_1[[i]][1], " ~ .")
    
    ITT_Model <- lm_robust(as.formula(f), data = data_for_analysis)
    
    
    names_of_columns_2 <- c(names_of_columns,'compliance_check_1')
    names_use <- names(Pulse_data_2)[(names(Pulse_data_2) %in% names_of_columns_2)]
    data_for_analysis <- Pulse_data_2[, names_use]
    
    #Clean Data:
    data_for_analysis <- Clean(data_for_analysis)
    
    f <- paste0(list_variables_to_run_1[[i]][1], '~ . - Treated | . - compliance_check_1')
    
    #CACE Model 1
    CACE_Model_1 <- iv_robust(as.formula(f), data = data_for_analysis)
    
    CACE_Model_1$term[which(CACE_Model_1$term == "compliance_check_1")] <- 'Treated'
    
    # #CACE Model 2 - Passed first and second wave compliance check
    names_of_columns_3 <- c(names_of_columns,'Complied')
    names_use <- names(Pulse_data_2)[(names(Pulse_data_2) %in% names_of_columns_3)]
    data_for_analysis <- Pulse_data_2[, names_use]
    #Clean Data:
    data_for_analysis <- Clean(data_for_analysis)
    f <- paste0(list_variables_to_run_1[[i]][1], '~  . - Treated | . - Complied')
    CACE_Model_2 <- iv_robust(as.formula(f), data = data_for_analysis)
    
    CACE_Model_2$term[which(CACE_Model_2$term == "Complied")] <- 'Treated'
    
    #names of the variables:
    Variable_Names <- names(ITT_Model$coefficients)[-1]
    Variable_Names <- str_replace_all(Variable_Names, top_attributes_html)
    for(z in 1:length(names(coef(ITT_Model)))){
      if(names(coef(ITT_Model))[z] == 'Treated'){
        ITT_coef = round(coef(ITT_Model)[z],3)
        ITT_SD = round(coef(summary(ITT_Model))[, "Std. Error"][z],3)
        ITT_P = coef(summary(ITT_Model))[, "Pr(>|t|)"][z]
        ITT_ci_upper = round(confint(ITT_Model)[z,2],3)
        ITT_ci_lower = round(confint(ITT_Model)[z,1],3)
      }
    }
    
    for(z in 1:length(names(coef(CACE_Model_1)))){
      if(names(coef(CACE_Model_1))[z] == 'compliance_check_11' | names(coef(CACE_Model_1))[z] == 'compliance_check_1'){
        C_1_coef = round(coef(CACE_Model_1)[z],3)
        C_1_SD = round(coef(summary(CACE_Model_1))[, "Std. Error"][z],3)
        C_1_P = coef(summary(CACE_Model_1))[, "Pr(>|t|)"][z]
      }
    }
    
    for(z in 1:length(names(coef(CACE_Model_2)))){
      if(names(coef(CACE_Model_2))[z] == 'Complied'){
        C_2_coef = round(coef(CACE_Model_2)[z],3)
        C_2_SD = round(coef(summary(CACE_Model_2))[, "Std. Error"][z],3)
        C_2_P = coef(summary(CACE_Model_2))[, "Pr(>|t|)"][z]
      }
    }
    
    Stars_I_P = ''
    if(ITT_P < 0.05){
      Stars_I_P = '*'
    }
    if(ITT_P < 0.01){
      Stars_I_P = '**'
    }
    if(ITT_P < 0.001){
      Stars_I_P = '***'
    }
    
    Stars_C_1 = ''
    if(C_1_P < 0.05){
      Stars_C_1 = '*'
    }
    if(C_1_P < 0.01){
      Stars_C_1 = '**'
    }
    if(C_1_P < 0.001){
      Stars_C_1 = '***'
    }
    
    Stars_C_2 = ''
    if(C_2_P < 0.05){
      Stars_C_2 = '*'
    }
    if(C_2_P < 0.01){
      Stars_C_2 = '**'
    }
    if(C_2_P < 0.001){
      Stars_C_2 = '***'
    }
    ITT_1 <- paste0(ITT_coef,' (',ITT_SD,')',Stars_I_P)
    CACE_1 <- paste0(C_1_coef,' (',C_1_SD,')',Stars_C_1)
    CACE_2 <- paste0(C_2_coef,' (',C_2_SD,')',Stars_C_2)
    Prop_F <- as.character(round(Deciles[x],3))
    Num_Obs <- nrow(data_for_analysis)
    new_matr <- rbind(new_matr,c(Decile_Names[x],Prop_F,ITT_1,CACE_1,CACE_2,Num_Obs))
    
    if(i == 5){
      ci_matr <- rbind(ci_matr,
                       c(Decile_Names[x],ITT_ci_lower,ITT_coef,ITT_ci_upper))
    }
    if(i == 10){
      ci_matr_2 <- rbind(ci_matr_2,
                         c(Decile_Names[x],ITT_ci_lower,ITT_coef,ITT_ci_upper))
    }
  }
  colnames(new_matr) <- c('Decile','Threshold','ITT Model','CACE Model (Weak Compliance Measure)','CACE Model (Strong Compliance Measure)','Sample Size')
  new_matr <- new_matr[-1,]
  
}




colnames(ci_matr) <- c('Decile','L_CI','Coef','U_CI')
ci_matr <- ci_matr[-1,]
ci_matr <- as.data.frame(ci_matr)

ci_matr$L_CI <- as.character(ci_matr$L_CI)
ci_matr$Coef <- as.character(ci_matr$Coef)
ci_matr$U_CI <- as.character(ci_matr$U_CI)
ci_matr$L_CI <- as.numeric(ci_matr$L_CI)
ci_matr$Coef <- as.numeric(ci_matr$Coef)
ci_matr$U_CI <- as.numeric(ci_matr$U_CI)

colnames(ci_matr_2) <- c('Decile','L_CI','Coef','U_CI')
ci_matr_2 <- ci_matr_2[-1,]
ci_matr_2 <- as.data.frame(ci_matr_2)

ci_matr_2$L_CI <- as.character(ci_matr_2$L_CI)
ci_matr_2$Coef <- as.character(ci_matr_2$Coef)
ci_matr_2$U_CI <- as.character(ci_matr_2$U_CI)
ci_matr_2$L_CI <- as.numeric(ci_matr_2$L_CI)
ci_matr_2$Coef <- as.numeric(ci_matr_2$Coef)
ci_matr_2$U_CI <- as.numeric(ci_matr_2$U_CI)


ci_matr_2$Period <- 'July 1st - July 13th'
ci_matr$Period <- 'Before July 1st'

Decile_ci <- rbind(ci_matr,ci_matr_2)

Decile_ci$Decile <- gsub('Whole Sample','Bottom 100 Percent',Decile_ci$Decile)
Decile_ci$Decile <- gsub('Bottom ','',Decile_ci$Decile)
Decile_ci$Decile <- gsub(' Percent','%',Decile_ci$Decile)


Decile_ci$Decile <- factor(Decile_ci$Decile,levels=c('100%',
                                                     '90%',
                                                     '80%',
                                                     '70%',
                                                     '60%',
                                                     '50%',
                                                     '40%',
                                                     '30%',
                                                     '20%',
                                                     '10%'))

Decile_ci <- Decile_ci %>% arrange(Decile,Period)


#Decile Figure


Decile_ci$x <-c(0.2,0.3,0.5,0.6,0.8,0.9,1.1,1.2,1.4,1.5,1.7,1.8,2.0,2.1,2.3,2.4,2.6,2.7,2.9,3.0)

ggplot(data = Decile_ci, aes(x = x, y = Coef)) +
  geom_hline(aes(yintercept = 0), color = "gray",
             linetype = 2, size = 1.2) +
  geom_point(aes(color = Period, shape=Period),size=4) +
  geom_linerange(aes(min = L_CI, 
                     max = U_CI, 
                     color = Period),
                 size=1.5) +
  scale_color_manual(values=c('red','blue','purple'), name = "Period") +
  ylab("Effect of NewsGuard Intervention on Average \n Reliability Score of News Diet (0-100) \n") +
  theme_classic() +
  theme(axis.title.x = element_text(size=16),
        axis.text.x  = element_text(size=12),
        axis.title.y = element_text(size=16),
        axis.text.y  = element_text(size=14),
        plot.title = element_text(size = 14),
        legend.title = element_text(size=14),
        legend.text = element_text(size=12)) +
  ylim(-0.75,10) +
  scale_x_continuous("\n Percentage of Respondents Included in Subsample\n(The Pre-Treatment Average News Reliability Score Decile Cutpoint is Listed in Parentheses)",
                     breaks=c(0.25,0.55,0.85,1.15,1.45,1.75,2.05,2.35,2.65,2.95),labels=c('100%\n(<100.0)',
                                                                                          '90%\n(<95)',
                                                                                          '80%\n(<92.5)',
                                                                                          '70%\n(<90.8)',
                                                                                          '60%\n(<90.0)',
                                                                                          '50%\n(<89.2)',
                                                                                          '40%\n(<88.4)',
                                                                                          '30%\n(<87.5)',
                                                                                          '20%\n(<85.3)',
                                                                                          '10%\n(<78.3)'),limits=c(0.15,3.05))


#Save Figure 5 in the main text:
ggsave('./Figures/Decile_Figure.png',height=10,width=12)













